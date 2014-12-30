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
