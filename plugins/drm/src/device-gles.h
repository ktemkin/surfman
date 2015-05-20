/*
 * Copyright (C) 2014-2015 Assured Information Security, Inc.
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


#include <gbm.h>
#include <GLES2/gl2.h>
#include <GLES2/gl2ext.h>
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <xf86drm.h>
#include <xf86drmMode.h>

//Compatibility "shim":
//Older versions of mesa define the unpack extensions without the standards-compliant _EXT suffix.
//If we have these older definitions (but not the compliant newer ones), we'll define the newer ones
//instead. This should let us work no matter the Mesa version.
#if !defined(GL_UNPACK_ROW_LENGTH_EXT) && defined(GL_UNPACK_ROW_LENGTH)
  #define GL_UNPACK_ROW_LENGTH_EXT  GL_UNPACK_ROW_LENGTH
  #define GL_UNPACK_SKIP_ROWS_EXT   GL_UNPACK_SKIP_ROWS
  #define GL_UNPACK_SKIP_PIXELS_EXT GL_UNPACK_SKIP_PIXELS
#endif

/**
 * The total color depth (bits describing color information) used for the DRM display.
 * This sets the color depth used to /output/ our DRM data; it is not related to the guest's color depth.
 */
static const int drm_gles_color_depth = 24;

/**
 * The total bits-per-pixel used for the DRM display.
 * This sets the format of the GBM buffers used to /output/ our DRM data; it is not related to the guest's color depth.
 */
static const int drm_gles_bits_per_pixel = 32;

/**
 * Defines whether the active platform prefers to receive new textures vs.
 * accepting partial textures (e.g. "dirty rectangles") for uploads. 
 * 
 * For now, we'll assume all platforms prefer new ones (which can be easily DMA'd),
 * as this is generally true of the mesa implementation we rely on. 
 */
static const int platform_prefers_new_textures = 1;


//TODO: Document the below!

struct gbm_state {
	struct gbm_device * dev;
	struct gbm_surface * surface;
  struct gbm_bo * previous_bo;
};


// Attribute index.
enum {
    ATTRIB_VERTEX,
    ATTRIB_TEXTUREPOSITON,
    NUM_ATTRIBUTES
};

// Uniform index.
enum {
    UNIFORM_DISPLAY_CONTENT,
    NUM_UNIFORMS
};


struct egl_state{
	EGLDisplay display;
	EGLConfig config;
	EGLContext context;
	EGLSurface surface;

  GLuint program;
  GLuint canvas_texture;
  GLuint canvas_width;
  GLuint canvas_height;
  GLenum canvas_color_layout;

  GLint uniforms[NUM_UNIFORMS];

};


struct gles_display {

  //The DRI fd with which this display object is associated.
  int dri_fd;

  //The state of the Generic Buffer Mangaer, which handles the actual frontend buffers.
  struct gbm_state gbm;

  //The state of EGL, the OpenGL ES Native Platform Library.
  struct egl_state egl;

};


struct drm_fb {
	struct gbm_bo *bo;
	uint32_t fb_id;
  uint32_t fd;
};
