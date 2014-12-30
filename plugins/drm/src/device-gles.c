/*
 * Copyright (C) 2014-2015 Assured Information Security, Inc.
 * Copyright (c) 2014 Citrix Systems, Inc.
 * Portions Copyright (c) 2012 Rob Clark <rob@ti.com> (from KMSCube)
 * Portions Copyright (c) 2010 Sunset Lake Software  (from their Color Tracking demo)
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
 *
 * -------------
 * device-gles.c
 *
 * An OpenGL ES and EGL based rendering device, which is intended to greatly
 * genericize the DRM plugin-- in many cases, without sacrificing hardware
 * acceleration. This likely won't be as efficient as per-driver implementations,
 * but is very close to GPU-agnostic, so it should be a lot more maintainable
 * and should greatly expand Surfman's compatibility.
 *
 */

#include "project.h"
#include "device-gles.h"

//A list of the graphics cards which can be supported under the OpenGL device driver.
//This should likely be replaced with a more reliable way to detect OpenGL ES.
enum supported_gles_device {
    SUPPORTED_GLES_DEVICE_RADEON = 0,
    SUPPORTED_GLES_DEVICE_NOUVEAU = 1,
    SUPPORTED_GLES_DEVICE_COUNT
};

//The udev "driver" string for each of the supported graphics cards. 
static const char * supported_gles_device_driver[SUPPORTED_GLES_DEVICE_COUNT] = {
    [SUPPORTED_GLES_DEVICE_RADEON] = "radeon",
    [SUPPORTED_GLES_DEVICE_NOUVEAU] = "nouveau"
};

/**
 * Initializes GBM, the Generic Buffer manager, for the given DRM surface.
 *
 * @param gbm A pointer to the structure which will store the GBM state. This will be populated
 *    with the information necessary to track this GBM session.
 * @param dri_fd The file descriptor being used to connect to DRI.
 * @param width The width of the GBM buffer to create. This should usually match the monitor's
 *    preferred mode, and need not match the target framebuffer size. Scaling will be handled
 *    automatically by OpenGL.
 * @param height The height of the GBM buffer to create. Like the width, this should likely also 
 *    match the monitor's preferred mode.
 */
static int set_up_gbm(struct gbm_state * gbm, int dri_fd, uint32_t width, uint32_t height)
{
  //Create a new connection to the Generic Buffer Manager.
	gbm->dev = gbm_create_device(dri_fd);

  //And create a new surface for the given screen.
	gbm->surface = gbm_surface_create(gbm->dev, width, height, GBM_FORMAT_XRGB8888, GBM_BO_USE_SCANOUT | GBM_BO_USE_RENDERING);

	if (!gbm->surface) {
		DRM_ERR("Failed to create a GBM surface on fd (%u).\n", dri_fd);
		return SURFMAN_ERROR;
	}

	return SURFMAN_SUCCESS;
}

/**
 * Returns GL_TRUE iff the given shader appears to have compiled successfully.
 *
 * @param shader The shader object to check.
 * @param name A short-hand name for the given shader; used in error messages,
 *    or NULL to disable error logging.
 *
 */
static GLboolean shader_compiled_succesfully(GLuint shader, const char * name) {

  int ret;

  //Check to see if we were correctly able to dynamically compile
  //the shader. If not, print out the relevant error message.
	glGetShaderiv(shader, GL_COMPILE_STATUS, &ret);
	if (!ret) {
		char *log;

    if(name) {
      DRM_ERR("Failed to create the %s shader!\n", name);

      //Determine the length of the shader message, if one was provided.
      glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &ret);

      //If a shader message was provided, print it to the console.
      if (ret > 1) {
        log = malloc(ret);
        glGetShaderInfoLog(shader, ret, NULL, log);
        DRM_ERR("%s\n", log);
        free(log);
      }
    }

    //And fail to start the application.
		return GL_FALSE;
	}

  return GL_TRUE;
}

/**
 * Creates and compiles a shader object from its source code.
 *
 * @param shader_type The type of shader to be created, e.g. GL_VERTEX_SHADER.
 * @param source The source code string for the given shader.
 * @param name A short string describing the shader, or NULL to disable error logging.
 *
 * @return A shader object handle, or zero on failure.
 *
 */
static GLint create_shader(GLenum shader_type, const char ** source, const char * name) {

  //Create a new vertex shader object, populated with the trivial
  //shader script above.
	GLint shader = glCreateShader(shader_type);

  //If we could not create the vertex shader, return indicating failure.
  if(shader == 0) {
    return shader;
  }

  //Populate the shader's source, and perform the compilation.
	glShaderSource(shader, 1, source, NULL);
	glCompileShader(shader);

  //Check to see if we were correctly able to dynamically compile
  //the shader. If not, print out the relevant error message.
  if(shader_compiled_succesfully(shader, name) == GL_FALSE) {
    return 0;
  }

  return shader;
}


/**
 * Creates a new Vertex shader for the 2D Display program.
 *
 * @return A handle for the vertex shader for this program, or 0 on failure.
 */
static GLint create_display_vertex_shader() {

  //Trivial OpenGL shader program, which simply creates the
  //2D drawing canvas.
	static const char * vertex_shader_source =
    "attribute vec4 position;                         \n"
    "attribute vec4 inputTextureCoordinate;           \n"
    "                                                 \n"
    "varying vec2 textureCoordinate;                  \n"
    "                                                 \n"
    "void main()                                      \n"
    "{                                                \n"
    "  gl_Position = position;                        \n"
    "  textureCoordinate = inputTextureCoordinate.xy; \n"
    "}                                                \n";

  //Create a new vertex shader object, populated with the trivial
  //shader script above.
  return create_shader(GL_VERTEX_SHADER, &vertex_shader_source, "vertex");

}

/**
 * Creates a new fragment shader for the 2D Display program.
 *
 * @return A handle for the fragment shader for this program, or 0 on failure.
 *
 */
static GLint create_display_fragment_shader() {

	static const char *fragment_shader_source =
    "varying highp vec2 textureCoordinate;                            \n"
    "                                                                 \n"
    "uniform sampler2D displayContent;                                \n"
    "                                                                 \n"
    "void main()                                                      \n"
    "{                                                                \n"
    "  gl_FragColor = texture2D(displayContent, textureCoordinate);   \n"
    "}                                                                \n";

  //Create a new vertex shader object, populated with the trivial
  //shader script above.
  return create_shader(GL_FRAGMENT_SHADER, &fragment_shader_source, "fragment");

}

/**
 * Create a simple OpenGL program designed to display a 2D buffer onscreen.
 *
 * @param vertex_shader A compiled vertex shader object, intended to be linked to
 *    the program.  @see create_display_vertex_shader
 * @param fragment_sh ader A compiled fragment shader object, intended to be linked
 *    to the program. @see create_display_fragment_shader
 *
 * @return A handle for the linked OpenGL 2D display program, or 0 on failure.
 */
static GLint create_display_program(GLint vertex_shader, GLint fragment_shader) {

  int ret;

  //Combine the new shaders into an OpenGL program...
	GLint program = glCreateProgram();
	glAttachShader(program, vertex_shader);
	glAttachShader(program, fragment_shader);

  //... bind our external parameters...
  glBindAttribLocation(program, ATTRIB_VERTEX, "position");
  glBindAttribLocation(program, ATTRIB_TEXTUREPOSITON, "inputTextureCoordinate");

  //... and link the program.
	glLinkProgram(program);

  //If linking the program failed, report the error to the user.
	glGetProgramiv(program, GL_LINK_STATUS, &ret);
	if (!ret) {
		char *log;

		DRM_ERR("Could not link the OpenGL program!\n");

    //... and if we have a more detailed message, print that.
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &ret);
		if (ret > 1) {
			log = malloc(ret);
			glGetProgramInfoLog(program, ret, NULL, log);
			DRM_ERR("%s", log);
      free(log);
		}

		return 0;
	}

  return program;
}

/** 
 * Create the texture object which will be used to render surfaces to the screen.
 * These are essentially our 2D image container.
 *
 * @return A handle corresponding to the texture used for 2D display.
 */
static GLuint create_canvas_texture() {

  GLuint texture;

  //Generate a new GL texture object, and retrieve its handle.
	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);

  //Set up the texture so it can be displayed across the whole screen.
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  return texture;
}

/**
 * Creates the main "2D" object used to display screens.
 * Uses the current OpenGL context.
 */
static void set_up_canvas_vertices() {

  //Create a simple set of vertices describing a rectangle,
  //which will stretch across the screen and provide our 2D
  //drawing canvas. This will be created out of tesselated
  //triangles by the code below.
  static const GLfloat squareVertices[] = {
        -1.0f, -1.0f,
        1.0f, -1.0f,
        -1.0f,  1.0f,
        1.0f,  1.0f,
  };

  //... and bind those vertices to our OpenGL program.
	glVertexAttribPointer(ATTRIB_VERTEX, 2, GL_FLOAT, 0, 0, squareVertices);
	glEnableVertexAttribArray(ATTRIB_VERTEX);
}


/**
 * Creates (and attaches) the textures which will be used to display
 * VM screens.  Uses the current OpenGL context.
 */
static void set_up_canvas_textures(struct gles_display * display) {

  //And specify the position for a texture object which will
  //span the whole screen. By rendering into this texture,
  //we'll effectively perform a GPU-neutral copy of the
  //framebuffer to video memory.
  static const GLfloat textureVertices[] = {
        0.0f, 1.0f,
        1.0f, 1.0f,
        0.0f, 0.0f,
        1.0f, 0.0f,
  };

  //Create the core texture object...
  display->egl.canvas_texture = create_canvas_texture();

  //... and pin it to the given set of vertices.
	glVertexAttribPointer(ATTRIB_TEXTUREPOSITON, 2, GL_FLOAT, 0, 0, textureVertices);
	glEnableVertexAttribArray(ATTRIB_TEXTUREPOSITON);
}

/**
 * Sets up a large, blank "canvas" rectangle on the OpenGL screen,
 * applies a basic texture to it, and
 *
 * @param gles_display The display object on which the program will be displayed.
 * @param gl_program The OpenGL program which will handle 2D display.
 *    @see create_display_program()
 *
 */
static void set_up_drawing_canvas(struct gles_display * display, GLint gl_program) {

  //Enable the prorgam which performs the core drawing canvas rendering.
  //(e.g. configuers the main texture to be displayed on a fullscreen
  // square "canvas")
	glUseProgram(gl_program);
	glUniform1i(display->egl.uniforms[UNIFORM_DISPLAY_CONTENT], 0);

  //And set up the object and texture that composes the canvas.
  set_up_canvas_vertices();
  set_up_canvas_textures(display);
};


/**
 * Initializes EGL, which provides platform support for OpenGL. This is the "glue" which
 * allows us to use OpenGL ES directly on top of DRM (via GBM).
 *
 * @param gles_display The display object on which we want to set up EGL.
 */
static int set_up_egl(struct gles_display * display)
{
	EGLint major, minor, n;
	GLuint vertex_shader, fragment_shader;

  //Grab a convenient shortcut to the EGL state.
  struct egl_state * egl = &display->egl;

  //Specify the minimum requirements for our EGL instance.
  //We need to know that the EGL implementation supports the
  //functioanlity we're using, but that's about it.
	static const EGLint context_attribs[] = {
		EGL_CONTEXT_CLIENT_VERSION, 2,
		EGL_NONE
	};

  //Specify the minimum acceptable parameters for an EGL configuration.
  //We'll accept pretty much anything that we can render to (the _BIT)
  //directives-- and which is in color. OpenGL ES will abstract away
  //the actual graphics formats-- this will allow us to fall back nicely
  //on (embedded?) systems which don't support the color modes we need.
	const EGLint config_attribs[] = {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RED_SIZE, 1,
		EGL_GREEN_SIZE, 1,
		EGL_BLUE_SIZE, 1,
		EGL_ALPHA_SIZE, 0,
		EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
		EGL_NONE
	};

  //Convert the GBM surface into an EGL drawing "window";
  //this will allow OpenGL to render to the given GBM surface.
	egl->display = eglGetDisplay(display->gbm.dev);

  //And initialize EGL on the given display.
	if (!eglInitialize(egl->display, &major, &minor)) {
		DRM_ERR("Failed to initialize EGL!");
		return SURFMAN_ERROR;
	}

	//Display some nice information for those following along (debugging) at home.
  DRM_INF("Using display %p with EGL version %d.%d\n", egl->display, major, minor);
	DRM_INF("EGL Version \"%s\"\n", eglQueryString(egl->display, EGL_VERSION));
	DRM_INF("EGL Vendor \"%s\"\n", eglQueryString(egl->display, EGL_VENDOR));
	DRM_INF("EGL Extensions \"%s\"\n", eglQueryString(egl->display, EGL_EXTENSIONS));

  //Select the API we'll be using with EGL, which is OpenGL ES.
	if (!eglBindAPI(EGL_OPENGL_ES_API)) {
		error("failed to bind api EGL_OPENGL_ES_API\n");
		return SURFMAN_ERROR;
	}

  //Attempt select an EGL configuration which meets our needs-- this configuration
  //will be used to wrap the GBM buffer with fancy GL rendering objects.
	if (!eglChooseConfig(egl->display, config_attribs, &egl->config, 1, &n) || n != 1) {
		error("failed to choose config: %d\n", n);
		return SURFMAN_ERROR;
	}

  //Using the configuration, create an EGL context, which we'll use for display and
  //rendering.
	egl->context = eglCreateContext(egl->display, egl->config, EGL_NO_CONTEXT, context_attribs);
	if (egl->context == NULL) {
		error("failed to create context\n");
		return SURFMAN_ERROR;
	}

  //Create an on-screen rendering surface; this is what we'll use to do our actual
  //rendering!
	egl->surface = eglCreateWindowSurface(egl->display, egl->config, display->gbm.surface, NULL);
	if (egl->surface == EGL_NO_SURFACE) {
		error("failed to create egl surface\n");
		return SURFMAN_ERROR;
	}

	//And attach this surface to our existing context.
  eglMakeCurrent(egl->display, egl->surface, egl->surface, egl->context);

  //We'll use a pair of shader objects to run a simple OpenGL program,
  //whose sole responsibility will be to draw 2D data to the screen. This
  //roundabout program gives us a generic way to achieve hardware acceleration!
  //
  //Create the shader objects necessary to run the program.
  vertex_shader   = create_display_vertex_shader();
  fragment_shader = create_display_fragment_shader();

  //If we failed to generate either of the two shaders, fail.
  if(!fragment_shader || !vertex_shader) {
    return -1;
  }

  //Create the actual OpenGL program that displays the 2D texture.
  egl->program = create_display_program(vertex_shader, fragment_shader);
  if(!egl->program) {
    return -1;
  }

  //Delete our "local references" to the shaders.
  //
  //This decreases their internal reference count, but won't
  //immediately remove the shaders, as they're stilled referenced
  //by the program. Rather, this ensures they're deleted when
  //the program releases its reference to them!
  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);

	// Update attribute values.
  set_up_drawing_canvas(display, egl->program);

	return SURFMAN_SUCCESS;
}

/**
 * Destroys a given EGL instance, allowing its GBM object to be freed or reused.
 *
 * @param gles_display The display object whose EGL instance we want to destroy.
 */
static void tear_down_egl(struct gles_display * display) {

  //Detach the current program ("use no program"), and then mark the OpenGL program
  //we're using for deletion.
  glUseProgram(0);
  glDeleteProgram(display->egl.program);

  //Ensure that none of our surfaces or contexts are current-- or they'll be held on to
  //after they're marked for destruction!
  eglMakeCurrent(display->egl.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

  //Destroy the EGL surface and context we created for this display.
  if(eglDestroySurface(display->egl.display, display->egl.surface) != EGL_TRUE) {
    DRM_ERR("Failed to destroy the EGL surface!");
  }
  if(eglDestroyContext(display->egl.display, display->egl.context) != EGL_TRUE) {
    DRM_ERR("Failed to destroy the EGL context!");
  }

  //And terminate the EGL display connection. This will in turn destroy all of the EGL
  //and OpenGL ES resources!
  if(eglTerminate(display->egl.display) != EGL_TRUE) {
    DRM_ERR("Failed to terminate the EGL display.");
  }

  //Finally, return EGL to its initial state.
  eglReleaseThread();
}

/**
 * Destroys a GBM device object, allowing the screen to be reused.
 *
 * @param gles_display The display object whose GBM device object we want to destroy.
 */
static void tear_down_gbm(struct gles_display * display) {

  //Destroy the active GBM device surface...
  gbm_surface_destroy(display->gbm.surface);

  //... and destroy our GBM device object.
  gbm_device_destroy(display->gbm.dev);
}


/**
 * Callback function which is called automatically when a GBM BO
 * is destroyed (when configured appropriately below).
 *
 * @param bo The GBM buffer object-- necessary to match the required 
 *    callback signature.
 * @param data The data being passed to the callback-- which should be the
 *    address of the drm_fb to be destroyed.
 */
static void drm_fb_destroy_callback(struct gbm_bo *bo, void *data)
{
  //Ensure that the unused bo term doesn't raise any compiler warnings.
  (void) bo;

  //Retreive the drm_fb object that is to be destroyed.
  struct drm_fb *fb = data;

  //If this device doesn't seem to have a valid framebuffer object,
  //skip it.
  if(!data) {
    return;
  }

  //If this GBM device had a framebuffer attached, destroy it!
	if (fb->fb_id) {
		drmModeRmFB(fb->fd, fb->fb_id);
  }

  //... and free its memory.
	free(fb);
}



/**
 * Get a DRM framebuffer object from a GBM buffer object.
 * This core object allows us to render to the screen!
 *
 * @param display The GLES display object that corresponds to the GBM display.
 * @param bo The buffer object for which we'd like to find a DRM framebuffer.
 *
 * @return A structure containing the information about the located framebuffer.
 */
static struct drm_fb * get_drm_fb_for_bo(struct gles_display * display, struct gbm_bo *bo)
{

	uint32_t width, height, stride, handle;
	int ret;

  //If the BO already has an attached framebuffer, it will
  //be available as "user data". We'll use it-- this is a common
  //case, as GBM is capable of reusing BOs.
	struct drm_fb * fb = gbm_bo_get_user_data(bo);

  //If we retreived an existing framebuffer, return it directly.
	if (fb) {
		return fb;
  }

  //Otherwise, allocate a new block of memory for the framebuffer.
  //Note that this isn't destroyed here, as we're attaching this
  //to the buffer object! Rather, it will be automatically destroyed
  //by the callback provided below.
	fb = calloc(1, sizeof(struct drm_fb));
	fb->bo = bo;
  fb->fd = display->dri_fd;

  //Retreive the BO parameters that we'll need to pass to DRM.
	width  = gbm_bo_get_width(bo);
	height = gbm_bo_get_height(bo);
	stride = gbm_bo_get_stride(bo);
	handle = gbm_bo_get_handle(bo).u32;

  //And pass DRM the framebuffer.
  //TODO: Remove these magic numbers (color depth and BPP).
	ret = drmModeAddFB(display->dri_fd, width, height, 24, 32, stride, handle, &fb->fb_id);
	if (ret) {
		DRM_ERR("Failed to create a DRM fb: %s\n", strerror(errno));
		free(fb);
		return NULL;
	}

  //Once we've created the DRM framebuffer, attach it to the BO object for later
  //re-use.
	gbm_bo_set_user_data(bo, fb, drm_fb_destroy_callback);

  //And return the framebuffer itself.
	return fb;
}


/**
 * Bring the EGL-configured monitor online, and display the initial blank screen.
 *
 * @param display The GLES display which will be attached to the given monitor.
 * @param monitor The monitor onto which we want to display.
 */
static int initialize_monitor(struct gles_display * display, struct drm_monitor * monitor)
{
    struct gbm_bo *bo;
    struct drm_fb *fb;

    int ret;

    //Start off by clearing the screen. This allows us to get a
    //buffer for the first time! [Note that this also causes a potentially
    //irritaiting momentary "black flash" when switching. In most cases, this
    //is too fast to see, so it's likely not worth getting rid of. This section
    //would need to be rearchitectured anyways if we want to provide support for,
    //say, smooth transition animations.)
    glClearColor(0, 0, 0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    eglSwapBuffers(display->egl.display, display->egl.surface);

    //Get a GBM buffer object.
    bo = gbm_surface_lock_front_buffer(display->gbm.surface);
    if(!bo) {
      error("Got a null buffer object from lock-front-buffer on start up. Ut-oh!");
      return SURFMAN_ERROR;
    }

    //Retreive a DRM framebuffer object for the given GBM buffer object.
    fb = get_drm_fb_for_bo(display, bo);

    //And "modeset" it onto the screen! [sic on surfman's part "prefered"]
    ret = drmModeSetCrtc(monitor->device->fd, monitor->crtc, fb->fb_id, 0, 0, &monitor->connector, 1, &monitor->prefered_mode);
    if (ret) {
      DRM_ERR("Initializing monitor-- failed to modeset%s\n", strerror(errno));
      DRM_ERR("Mode was %ux%u", monitor->prefered_mode.hdisplay, monitor->prefered_mode.vdisplay);
      return SURFMAN_ERROR;
    }

    return SURFMAN_SUCCESS;
}


/**
 * Brings a GLES dispaly into existinence, effectively assigning a surface to a monitor.
 *
 * @param monitor The monitor onto which we want to display.
 * @param surface The surface which should be displayed on the target monitor.
 *
 * @return SURFMAN_SUCCESS on sucess, or SURFMAN_ERROR on any failure.
 */
static int gles_set(struct drm_monitor *monitor, struct drm_surface *surface)
{
  int dri_fd = monitor->device->fd;

  GLuint preferred_width  = monitor->prefered_mode.hdisplay;
  GLuint preferred_height = monitor->prefered_mode.vdisplay;
 
  //Allocate a new gles_display object for the current display.
  struct gles_display * display =  malloc(sizeof(struct gles_display));
  monitor->device_data = display;
  display->dri_fd = dri_fd;


  //Initialize a physical rendering surface using GBM.
  //Note that we create a GBM buffer which is the same size as the monitor's preferred mode, always-- 
  //OpenGL will automatically scale the rendered framebuffer to this mode in the event of a mismatch. 
  //This may not always be desired behavior-- but it will work for now.
  if(set_up_gbm(&(display->gbm), dri_fd, preferred_width, preferred_height)) {
    DRM_ERR("Failed to set up GBM!");
    return SURFMAN_ERROR;
  }

  //... and set up the OpenGL backend we'll use to render.
  if(set_up_egl(display)) {
    DRM_ERR("Failed to set up EGL!");
    return SURFMAN_ERROR;
  }

  //Finally, set up the display we're going to use.
  if(initialize_monitor(display, monitor)) {
    DRM_ERR("Failed to initialize the monitor!");
    return SURFMAN_ERROR;
  }

  //Add this monitor to the list of monitors managed by the DRM plugin.
  list_add_tail(&monitor->l_sur, &surface->monitors);
  monitor->surface = surface;

  DRM_INF("GLES display activated.");
  return SURFMAN_SUCCESS;

}

/**
 * Remove a GLES display-- effectively unassigning it from a monitor.
 *
 * @param monitor The monitor whose display we want to undo.
 */
static void gles_unset(struct drm_monitor *monitor)
{
    struct gles_display * display = monitor->device_data;
  
    //Tear down the EGL and GBM instances...
    tear_down_egl(display);
    tear_down_gbm(display);

    //... and free our gles_display object.
    free(monitor->device_data);
    monitor->device_data = NULL;

    //Finally, remove the monitor from our list of handled monitors.
    monitor->surface = NULL;
    list_del(&monitor->l_sur);

    DRM_INF("GLES display deactivated.");

}

/**
 * Returns the OpenGL pixel formats that should be used when transferring the provided
 * surface to a texture.
 *
 * @param surfman_surface_fmt The surfman-specific format code, which specifies how the pixels
 *    are represented in the surfman surface structure.
 *
 * @param gl_fmt A pointer to a GLenum object, which will be popualted with the surface's pixel format.
 * @param gl_typ A pointer to a GLenum object, which will be popualted with the surface's storage type.
 *
 * @return SURFMAN_SUCCESS if we support the given surface format, or SURFMAN_ERROR if we do not.
 */
static int get_gl_formats(int surfman_surface_fmt, GLenum *gl_fmt, GLenum *gl_typ)
{
  switch (surfman_surface_fmt) {

  case SURFMAN_FORMAT_BGRX8888:
      *gl_fmt = GL_BGRA_EXT;
      *gl_typ = GL_UNSIGNED_BYTE;
      return SURFMAN_SUCCESS;

  case SURFMAN_FORMAT_RGBX8888:
      *gl_fmt = GL_RGBA;
      *gl_typ = GL_UNSIGNED_BYTE;
      return SURFMAN_SUCCESS;

  case SURFMAN_FORMAT_BGR565:
      error("OpenGL ES does not support BGR565, and we've yet to implement manual translation. You're out of luck!");
      return SURFMAN_ERROR;
  default:
      error("unsupported surfman surface format %d", surfman_surface_fmt );
      return SURFMAN_ERROR;
  }
}

/**
 * Uploads the provided bitmap onto the drawing canvas. Hardware accelerated, in most cases.
 *
 * @param display The gles_device that holds the EGL instance for the current state.
 * @param width The width of the image to be uploaded.
 * @param height The height of the image to be uploaded.
 * @param color_layout The color layout (e.g. RGBA) for the image to be uploaded-- these can be decoded from surfman image formats using get_gl_formats.
 * @param storage_type The storage type (e.g. GL_UNSIGNED_BYTE) for the image to be uploaded-- again, these can be retreived using get_gl_formats.
 * @param target_bitmap The bitmap to be uploaded.
 */
static void update_canvas(struct gles_display * display, GLuint width, GLuint height, GLenum color_layout, GLenum storage_type, GLubyte * target_bitmap, const struct rect * area_to_update) {

  //Check to see if the target bitmap matches the width and height of the buffer already craeted on the canvas.
  int width_changed  = (width  != display->egl.canvas_width);
  int height_changed = (height != display->egl.canvas_height);

  //TODO: Possibly check the color mode of the canvas, too?

  //Specify the texture that we're working with.
	glBindTexture(GL_TEXTURE_2D, display->egl.canvas_texture);

  //For now, we'll ignore the area to update; as updating
  //subsections currently winds up being slower on some OpenGL 
  //ES implementations.
  //
  //(Perhaps someone with more knowledge of GL can identify
  // if this should always be the case-- and if there's a better
  // way to work around that?)
  (void)area_to_update;

  //If the size of the image to be applied has changed, use glTexImage2D, which reallocates the texture's
  //memory accordinglydisplay.
  if(width_changed || height_changed) {
    glTexImage2D(GL_TEXTURE_2D, 0, color_layout, width, height, 0, color_layout, storage_type, target_bitmap);
    display->egl.canvas_width  = width;
    display->egl.canvas_height = height;
  }
  //Otherwise, use glTexSubImage2D, which replaces the texture's contents without reallocation-- which
  //is thus a bit faster!
  else {
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, color_layout, storage_type, target_bitmap);
  }

}

/**
 * Render the canvas to the current render buffer. This uses the current canvas bitmap;
 * which can be updated with update_canvas().
 */
static void render_canvas() {
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}


/**
 * Upload the provided surface onto the given GLES display's GPU. This typically is implemented by having OpenGL
 * set up a DMA transfer in the target card; but can also theroetically be implemented via a plain copy. (I'm looking
 * at you, swrast).
 *
 * @param display The GLES display onto which the given surface should be copied.
 * @param surface_to_display The surfman surface whose contents should be copied to the GPU.
 * @param area_to_update The screen area which requires an update, if known, or NULL if the whole screen should be updated.
 */ 
static void upload_to_gpu(struct gles_display * display, const struct drm_surface * surface_to_display, const struct rect * area_to_update)
{
    GLenum fb_format, fb_type;

    //Attempt to get GL-compatible information about the pixel and storage formats used by the target surface.
    if (get_gl_formats(surface_to_display->fb.format, &fb_format, &fb_type ) != SURFMAN_SUCCESS) {  
        error("Unsupported/unknown surface pixel encoding 'format'!");
        return;
    }

    //Perform the actual copy from the surface to the canvas.
    update_canvas(display, surface_to_display->fb.width, surface_to_display->fb.height, fb_format, fb_type, surface_to_display->fb.map, area_to_update);
}

/** 
 * Refresh the given display. This is called whenever the display needs to be updated-- in most cases, surfman will call this 60 times per second.
 *
 * @param monitor The monitor whose display needs to be updated.
 * @param surface The surface currently being displayed on the monitor.
 * @param area_to_update The screen area which requires an update, if known, or NULL if the whole screen should be updated.
 * 
 */
static void gles_refresh(struct drm_monitor *monitor, const struct drm_surface *surface, const struct rect * area_to_update)
{
  struct gbm_bo * bo;
  struct drm_fb *fb;

  struct gles_display * output_display = monitor->device_data;

  // Upload the surface's data to the GPU.
  upload_to_gpu(output_display, surface, area_to_update);

  // And render it onto an EGL buffer.
  render_canvas();

  // Swap the active EGL buffers, so our next render targets an offscreen buffer.
  // (This doesn't do much for us right now, as we don't double buffer the DRM output,
  //  but it will be nice when we eventually do.)
  eglSwapBuffers(output_display->egl.display, output_display->egl.surface);

  // Retreieve the GBM buffer that corresponds to the EGL buffer to be displayed.
  bo = gbm_surface_lock_front_buffer(output_display->gbm.surface);
  if(!bo) {
    error("Failed to get a display buffer from GBM (instance %u)!", output_display->gbm.surface);
    return;
  }

  // And retrieve the actual DRM framebuffer associated with the GBM device.
  // (If no framebuffer exists, this will implicitly attach the GBM framebuffer to DRM).
  fb = get_drm_fb_for_bo(output_display, bo);

  // And apply that framebuffer directly to the screen. Ideally, we'd actually use a
  // VSYNC timed page flip to do this, but since currently the surface itself tears
  // when being uploaded to surfman, there's no point in adding the complexity.
  // I'll consider this a TODO.
  drmModeSetCrtc(monitor->device->fd, monitor->crtc, fb->fb_id, 0, 0, &monitor->connector, 1, &monitor->prefered_mode);

  // Now that we're no longer using the previous display frame, it's safe to release
  // it back to GBM for re-use. Do so!
  gbm_surface_release_buffer(output_display->gbm.surface, output_display->gbm.previous_bo);
  output_display->gbm.previous_bo = bo;
}

/**
 * Determines if the OpenGL ES driver can be used with the given device.
 * TODO: This should probably be more refined to actually check for support-- but this will do for
 * now (as most GPUs should be supported with swrast as a fallback).
 */
static int gles_match_udev_device(struct udev *udev, struct udev_device *device)
{
    const char *driver;
    struct udev_device *dev;
    int rc, i;

    //We only want to accept devices that have device nodes-- we need a device that we can control with DRI!
    if (!udev_device_get_devnode(device)) {
        DRM_DBG("%s has no devnode (likely udev subdevice of DRM subsystem).", udev_device_get_sysname(device));
        return EEXIST;
    }
    
    //Skip the DRI control device; we're looking for a device we can use directly with DRI.
    if (!strncmp(udev_device_get_sysname(device), "controlD64", sizeof ("controlD64") - 1)) {
        DRM_DBG("Ignoring redundant %s DRM device.", udev_device_get_sysname(device));
        return EEXIST;
    }

    //Get a reference to the driver for the device in use.
    dev = udev_device_new_from_drm_device(udev, device);
    if (!dev) {
        DRM_DBG("%s: udev_device_new_from_drm_device failed... (%s).", __FUNCTION__, strerror(errno));
        return ENODEV;
    }
    driver = udev_device_get_driver(dev);
  

    rc = SURFMAN_ERROR;

    //If we've retreived a valid device driver, compare it against each of the cards that we know will
    //work with MESA--- note that we can fall back to "swrast" if we need to.
    if(driver) {
      for(i = 0; i < SUPPORTED_GLES_DEVICE_COUNT; ++i) {
      
        //If we have a match, we can use this graphics card wit this genericized driver!
        if(strcmp(supported_gles_device_driver[i], driver) == 0) {
          DRM_INF("Discovered a GLES-compatible %s card; suggesting to surfman it be used with the DRM driver.", driver);
          rc = SURFMAN_SUCCESS;
          break; 
        }
      }
    }

    udev_device_unref(dev);
    return rc;
}


const struct drm_device_ops gles_ops = {
    .name    = "GLES",
    .set     = gles_set,
    .unset   = gles_unset,
    .refresh = gles_refresh,
    .match   = gles_match_udev_device
};

