/*
 * Copyright (c) 2014 Citrix Systems, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "project.h"

extern int use_foreign_framebuffers;

/* Constraints on modesetting with libDRM:
 * - We are only able to scale up if
 *   + Using plane,
 *   + We find an all around bigger mode than the framebuffer.
 * - We are not able to scale down.
 * - We can manage scaled up framebuffer using planes or direct modesetting,
 *   but it is more flexible using planes. */
static inline drmModeModeInfoPtr __find_mode(struct framebuffer *fb,
                                             drmModeModeInfoPtr modes,
                                             unsigned int count,
                                             drmModeModeInfoPtr *fallback_mode)
{
    unsigned int i;
    unsigned int dh, dw, dl, odl = ~0L;   /* delta-width, delta-height, delta-length, old_delta-lenght. */

    *fallback_mode = NULL;
    for (i = 0; i < count; ++i) {
        if ((modes[i].hdisplay == fb->width) &&
            (modes[i].vdisplay == fb->height)) {
            /* Found compatible mode. */
            return &modes[i];
        }
        if ((modes[i].hdisplay > fb->width) && (modes[i].vdisplay > fb->height)) {
            /* Lets avoid to crop. */
            *fallback_mode = &modes[i];
            odl = 0;
        } else {
            dh = max(modes[i].hdisplay, fb->width) - min(modes[i].hdisplay, fb->width);
            dw = max(modes[i].vdisplay, fb->height) - min(modes[i].vdisplay, fb->height);
            dl = dh * dh + dw * dw;
            if (dl < odl) {
                *fallback_mode = &modes[i];
                odl = dl;
            }
        }
    }
    return NULL;
}

static inline drmModeModeInfoPtr __find_mode_cropped(struct framebuffer *fb,
                                                     drmModeModeInfoPtr modes,
                                                     unsigned int count)
{
    unsigned int i;

    for (i = 0; i < count; ++i) {
        if ((modes[i].hdisplay <= fb->width) &&
            (modes[i].vdisplay <= fb->height)) {
            /* Found compatible mode. */
            return &modes[i];
        }
    }
    return &modes[0];
}

/* Helper with DRM plumbings to create a plane. */
static struct drm_plane *__plane_find_new(struct drm_device *device, uint32_t crtc)
{
    drmModeResPtr rs;
    drmModePlaneResPtr rp;
    drmModePlanePtr p;
    struct drm_plane *plane = NULL;
    unsigned int i;
    int j, err = 0;

    rs = drmModeGetResources(device->fd);
    if (!rs) {
        err = errno;
        DRM_DBG("Could not access device \"%s\" resources (%s).", device->devnode,
                strerror(errno));
        goto fail_rs;
    }
    rp = drmModeGetPlaneResources(device->fd);
    if (!rp) {
        err = errno;
        DRM_DBG("Could not access device \"%s\" planes resources (%s).", device->devnode,
                strerror(errno));
        goto fail_rp;
    }

    for (i = 0; i < rp->count_planes; ++i) {
        p = drmModeGetPlane(device->fd, rp->planes[i]);
        if (!p) {
            continue;
        }
        for (j = 0; j < rs->count_crtcs; ++j) {
            if (p->possible_crtcs & (1 << j)) {     /* We only have the id not the index in that array. */
                if (rs->crtcs[j] == crtc) {         /* Plane compatible with our crtc. */
                    if (!drm_device_plane_is_used(device, p->plane_id)) {
                        plane = drm_device_add_plane(device, p->plane_id);
                        if (!plane) {
                            err = errno;
                            DRM_DBG("Could not create a new plane on device \"%s\" (%s).",
                                    device->devnode, strerror(errno));
                        }
                        drmModeFreePlane(p);
                        goto out;
                    }
                }
            }
        }
        drmModeFreePlane(p);
    }

out:
    drmModeFreePlaneResources(rp);
fail_rp:
    drmModeFreeResources(rs);
fail_rs:
    errno = err;
    return plane;
}

/* Find a plane available for /framebuffer/ on /crtc_id/ and allocate a handler that can be displayed
 * using i915_plane_set. */
/* TODO: Poor design, crtc_id depends on the device, this is unclear just reading the arguments here.
 *       Framebuffers should refer to the monitor they are displayed onto, therefor planes
 *       would the CRTC to look for and the device it is on. */
static struct drm_plane *i915_plane_new(struct drm_framebuffer *framebuffer, uint32_t crtc_id)
{
    struct drm_plane *plane;
    int err;

    plane = __plane_find_new(framebuffer->device, crtc_id);
    if (!plane) {
        err = errno;
        DRM_DBG("No available plane on CRTC %u, device \"%s\" (%s).", crtc_id,
                framebuffer->device->devnode, strerror(errno));
        errno = err;
        return NULL;
    }
    plane->crtc = crtc_id;
    plane->framebuffer = framebuffer;
    plane->device = framebuffer->device;
    return plane;
}

static int i915_plane_set(struct drm_plane *plane)
{
    struct drm_device *d = plane->device;
    drmModeCrtcPtr c;
    struct framebuffer *fb = &plane->framebuffer->fb;
    unsigned int crtc_x, crtc_y, crtc_w, crtc_h;
    unsigned int fb_x, fb_y, fb_w, fb_h;
    int rc;

    c = drmModeGetCrtc(d->fd, plane->crtc);
    if (!c) {
        rc = -errno;
        DRM_DBG("Could not access device \"%s\" crtc %u (%s).", d->devnode, plane->crtc, strerror(errno));
        return rc;
    }
    if (c->mode.hdisplay < fb->width) {
        fb_x = (fb->width - c->mode.hdisplay) / 2;      /* Crop the framebuffer. */
        fb_w = c->mode.hdisplay;
        crtc_x = 0;                                     /* Spread CRTC as much as possible. */
        crtc_w = c->mode.hdisplay;
    } else {
        fb_x = 0;                                       /* Full frambuffer. */
        fb_w = fb->width;
        crtc_x = (c->mode.hdisplay - fb->width) / 2;    /* Center the CRTC on the FB. */
        crtc_w = fb->width;
    }
    if (c->mode.vdisplay < fb->height) {
        fb_y = (fb->height - c->mode.vdisplay) / 2;     /* Crop the framebuffer. */
        fb_h = c->mode.vdisplay;
        crtc_y = 0;                                     /* Spread CRTC as much as possible */
        crtc_h = c->mode.vdisplay;
    } else {
        fb_y = 0;                                       /* Full framebuffer. */
        fb_h = fb->height;
        crtc_y = (c->mode.vdisplay - fb->height) / 2;   /* Center the CRTC on the FB. */
        crtc_h = fb->height;
    }
    /* XXX: We don't /drm_device_set_master/ as we assume we're modeseting already.
     *      So it is, for now, caller's responsability to set_master. */
    rc = drmModeSetPlane(d->fd, plane->id, plane->crtc, plane->framebuffer->id, 0,
                         crtc_x, crtc_y, crtc_w, crtc_h,
                         fb_x << 16, fb_y << 16, fb_w << 16, fb_h << 16);
    if (rc) {
        rc = -errno;
        DRM_DBG("Could not set plane %d-%d-%d, %u,%u:%ux%u on device \"%s\" (%s).",
                plane->id, plane->crtc, plane->framebuffer->id, crtc_x, crtc_y, crtc_w, crtc_h,
                d->devnode, strerror(errno));
    }
    drmModeFreeCrtc(c);
    return rc;
}

static void i915_plane_release(struct drm_plane *plane)
{
    /* XXX: This call release the framebuffer (if bound already). */
    drm_device_del_plane(plane->device, plane->id);
}

/* I'm not sure we should expose those in the /drm_device_ops/ struct.
 * - /set/ and /unset/ will use them,
 * - having a framebuffer without putting it on the screen is not a use case yet. */
static struct drm_framebuffer *i915_framebuffer_new(struct drm_device *device, struct drm_surface *surface)
{
    struct drm_framebuffer *drmfb;
    int err;

    /* TODO: We now have a userland way to provide fbtap feature (and better) using
     *       DRM. So that check will eventually go away with fbtap. */
    if (surface->domid > 0 && use_foreign_framebuffers) {
        drmfb = framebuffer_foreign_ops.create(device, surface);
        if (drmfb) {
            goto succeed;
        }
        DRM_WRN("Could not create foreign framebuffer for dom%u (%s). Falling back to dumb method.",
                surface->domid, strerror(errno));
    } else {
        DRM_INF("Opting not to use a foreign framebuffer for dom%u.", surface->domid);
        DRM_INF("This can be controlled via the setting " PLUGIN_NAME "." SETTING_USE_FOREIGN_FRAMEBUFFER " in surfman.conf.");
    }

    /* We consider DUMB method is "fallback" as we assume it is supported for
     * all devices of our HCL (pretty safe bet though).
     * TODO: It is checkable at initialization, and we should do that. Also, REFRESH is currently disabled
     *       as it interfer with foreign method... So Dumb framebuffers won't refresh more than once. */
    drmfb = framebuffer_dumb_ops.create(device, surface);
    if (!drmfb) {
        err = errno;
        DRM_ERR("Could not create dumb framebuffer for dom%u (%s).", surface->domid, strerror(errno));
        errno = err;
        return NULL;
    }
    /* Creating a dumb framebuffer from a surface imply we need to map it. */
    err = drmfb->ops->map(drmfb);
    if (err) {
        DRM_ERR("Could not map dumb framebuffer for dom%u (%s).", surface->domid, strerror(-err));
        errno = -err;
        drmfb->ops->release(drmfb);
        return NULL;
    }
succeed:
    /* TODO: We might find useful to have a list of framebuffers for each device. */
    return drmfb;
}

static int i915_framebuffer_release(struct drm_framebuffer *framebuffer)
{
    /* XXX: Merely for interface, we could add some hooks around here too. */
    framebuffer->ops->release(framebuffer);
    return 0;
}

/*
 * Device interface.
 */
static int i915_modeset(struct drm_monitor *monitor, struct drm_framebuffer *drmfb)
{
    int rc = 0;
    drmModeConnector *con;
    drmModeModeInfoPtr mode, fallback_mode;
    unsigned int crtc_x = 0, crtc_y = 0;

    con = drmModeGetConnector(monitor->device->fd, monitor->connector);
    if (!con) {
        rc = -errno;
        DRM_ERR("Could not access connector %u on device \"%s\" (%s).",
                monitor->connector, monitor->device->devnode, strerror(errno));
        return rc;
    }
    mode = __find_mode(&drmfb->fb, con->modes, con->count_modes, &fallback_mode);
    if (!mode) {
        /* The monitor does not handle this mode, but if the framebuffer is smaller we can stick it in a
         * plane on top of a blanked framebuffer.
         * Lets try to use a blanked framebuffer as small as possible. */
        if (fallback_mode) {
            monitor->plane = i915_plane_new(drmfb, monitor->crtc);
            if (monitor->plane) {
                /* XXX: I couldn't setup a plane without an underlying (useless) framebuffer.
                 *      We allocate it here. */
                monitor->framebuffer = __dumb_framebuffer_create(monitor->device, fallback_mode->hdisplay,
                                                                 fallback_mode->vdisplay, drmfb->fb.depth,
                                                                 drmfb->fb.bpp);
                if (monitor->framebuffer) {
                    mode = fallback_mode;
                    goto modeset;   /* We're ready to modeset and compose the plane on top. */
                }
                /* Give up using a plane. */
                DRM_WRN("Could not create underlying framebuffer for device \"%s\" (%s).",
                        monitor->device->devnode, strerror(errno));
                monitor->plane->framebuffer = NULL;
                i915_plane_release(monitor->plane);
                monitor->plane = NULL;
            }
        }

         /* We couldn't setup a plane if the framebuffer is bigger or equal than the mode we might pull
          * this out by cropping at least on one dimension. Otherwise there's no point.*/
        mode = __find_mode_cropped(&drmfb->fb, con->modes, con->count_modes);
        if (drmfb->fb.width > mode->hdisplay) {
            crtc_x = (drmfb->fb.width - mode->hdisplay) / 2;
        }
        if (drmfb->fb.height > mode->vdisplay) {
            crtc_y = (drmfb->fb.height - mode->vdisplay) / 2;
        }
    }
    monitor->framebuffer = drmfb;

modeset:
    rc = drm_device_set_master(monitor->device);
    if (rc) {
        DRM_ERR("Cannot perform modeset operation while something else is mastering `%s' (%s).",
                monitor->device->devnode, strerror(-rc));
        goto fail_setmaster;
    }
    if (drmModeSetCrtc(monitor->device->fd, monitor->crtc, monitor->framebuffer->id,
                       crtc_x, crtc_y,
                       &(monitor->connector), 1, mode)) {
        rc = -errno;
        DRM_ERR("Cannot display framebuffer %u in connector %u (%s).",
                monitor->framebuffer->id, monitor->connector, strerror(errno));

        goto fail_setcrtc;
    }
    if (monitor->plane) {
        rc = i915_plane_set(monitor->plane);
        if (rc) {
            DRM_ERR("Could not set plane %u on connector %u (%s).",
                    monitor->plane->id, monitor->connector, strerror(-rc));
            goto fail_setplane;
        }
    }
    drm_device_drop_master(monitor->device);
    return 0;

fail_setplane:
    monitor->plane->framebuffer = monitor->framebuffer;  /* Don't release drmfb (caller's task). Trick to release vfb. */
    i915_plane_release(monitor->plane);
    monitor->plane = NULL;
fail_setcrtc:
    drm_device_drop_master(monitor->device);
fail_setmaster:
    drmModeFreeConnector(con);
    return rc;
}

static int i915_set(struct drm_monitor *monitor, struct drm_surface *surface)
{
    int rc;
    struct drm_framebuffer *drmfb;

    drmfb = i915_framebuffer_new(monitor->device, surface);
    if (!drmfb) {
        rc = -errno;
        DRM_DBG("Could not create a new framebuffer for dom%u on monitor %u (%s).",
                surface->domid, monitor->connector, strerror(errno));
        return rc;
    }
    rc = i915_modeset(monitor, drmfb);
    if (rc) {
        DRM_DBG("Count not setup dom%u framebuffer on monitor %u (%s).",
                surface->domid, monitor->connector, strerror(-rc));
        i915_framebuffer_release(drmfb);
        return rc;
    }
    list_add_tail(&monitor->l_sur, &surface->monitors);
    monitor->surface = surface;
    return 0;
}

static void i915_unset(struct drm_monitor *monitor)
{
    monitor->surface = NULL;
    list_del(&monitor->l_sur);
    if (monitor->plane) {
        i915_plane_release(monitor->plane);
        monitor->plane = NULL;
    }
    if (monitor->framebuffer) {
        i915_framebuffer_release(monitor->framebuffer);
        monitor->framebuffer = NULL;
    }
}

/* XXX: Different devices might be displaying only a plane, only a framebuffer or both,
 *      so we keep that refresh(monitor, surface) semantic even if for i915 we could
 *      directly have that logic in the plugin interface.
 *      This is to abstract the use of planes as framebuffers... */
static void i915_refresh(struct drm_monitor *monitor, const struct drm_surface *surface,
                         const struct rect *rectangle)
{
    struct drm_framebuffer *sink;

    /* XXX: We don't compose for now, so if there's a plane, that's what we want to display. */
    if (monitor->plane) {
        sink = monitor->plane->framebuffer;
    } else {
        sink = monitor->framebuffer;
    }
    /* Sanity test is the framebuffer's responsability. */
    sink->ops->refresh(sink, &surface->fb, rectangle);
}

static int i915_match_udev_device(struct udev *udev, struct udev_device *device)
{
    const char *driver;
    struct udev_device *dev;
    int rc;

    if (!udev_device_get_devnode(device)) {
        DRM_DBG("%s has no devnode (likely udev subdevice of DRM subsystem).", udev_device_get_sysname(device));
        return EEXIST;
    }
    /* XXX: Not sure what we could use that node for, but it comes with every card
     *      and usually is minor 64. */
    if (!strncmp(udev_device_get_sysname(device), "controlD64", sizeof ("controlD64") - 1)) {
        DRM_DBG("Ignoring redundant %s DRM device.", udev_device_get_sysname(device));
        return EEXIST;	/* We already, or will, have the other device node. */
    }

    dev = udev_device_new_from_drm_device(udev, device);
    if (!dev) {
        DRM_DBG("%s: udev_device_new_from_drm_device failed... (%s).", __FUNCTION__, strerror(errno));
        return ENODEV;
    }
    driver = udev_device_get_driver(dev);
    rc = !driver || strncmp(driver, "i915", sizeof ("i915") - 1);
    udev_device_unref(dev);
    return rc;
}

const struct drm_device_ops i915_ops = {
    .name = "i915",
    .set = i915_set,
    .unset = i915_unset,
    .refresh = i915_refresh,
    .match = i915_match_udev_device
};

