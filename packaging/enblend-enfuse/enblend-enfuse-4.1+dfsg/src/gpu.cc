/*
 * Copyright (C) 2004-2012 Andrew Mihal
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>

#include "global.h"
#include "gpu.h"


#ifdef HAVE_LIBGLEW

static const std::string command("enblend");

static GLuint GlutWindowHandle;
static GLint MaxTextureSize;
static GLuint PiTexture;
static GLuint ETexture;
static GLuint OutTexture;
static GLuint FB;
static GLhandleARB ProgramObject;
static GLhandleARB ShaderObject;
static GLint PiTextureParam;
static GLint ETextureParam;
static GLint TempParam;
static GLint KMaxParam;


static const char *GDAKernelSource = {
    "uniform sampler2DRect PiTexture;"
    "uniform sampler2DRect ETexture;"
    "uniform float Temperature;"
    "uniform float KMax;"
    "void main(void)"
    "{"
    "   vec4 pix = texture2DRect(PiTexture, gl_TexCoord[0].st);"
    "   vec4 ex = texture2DRect(ETexture, gl_TexCoord[0].st);"
    "   vec4 An;"
    "   vec4 pi_plus;"
    "   vec4 sum = vec4(0.0, 0.0, 0.0, 0.0);"
    "   float i = 0.0;"
    "   for (i = 0.0; i < KMax; i++) {"
    "       vec2 coord = vec2(i, gl_TexCoord[0].t);"
    "       An = exp((ex - texture2DRect(ETexture, coord)) / Temperature) + 1.0;"
    "       pi_plus = pix + texture2DRect(PiTexture, coord);"
    "       sum += (pi_plus / An);"
    "   }"
    "   gl_FragColor = sum / KMax;"
    "}"
};


void checkGLErrors(const char* file, unsigned line)
{
    const GLenum errCode = glGetError();

    if (errCode != GL_NO_ERROR) {
        std::cerr << command
             << ": OpenGL error in " << file << ":" << line << ": "
             << gluErrorString(errCode) << std::endl;
        exit(1);
    }
}


void printInfoLog(GLhandleARB obj)
{
    GLint infologLength = 0;
    GLint charsWritten = 0;
    char *infoLog;

    glGetObjectParameterivARB(obj, GL_OBJECT_INFO_LOG_LENGTH_ARB, &infologLength);
    if (infologLength > 1) {
        infoLog = new char[infologLength];
        glGetInfoLogARB(obj, infologLength, &charsWritten, infoLog);
        std::cerr << command << ": info: GL info log\n" << infoLog << std::endl;
        delete [] infoLog;
    }
}

bool checkFramebufferStatus()
{
    const GLenum status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);

    switch (status) {
    case GL_FRAMEBUFFER_COMPLETE_EXT:
        return true;
    case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT:
        std::cerr << command
             << ": GL error: Framebuffer incomplete, incomplete attachment"
             << std::endl;
        return false;
    case GL_FRAMEBUFFER_UNSUPPORTED_EXT:
        std::cerr << command
             << ": unsupported framebuffer format"
             << std::endl;
        return false;
    case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
        std::cerr << command
             << ": framebuffer incomplete, missing attachment"
             << std::endl;
        return false;
    case GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
        std::cerr << command
             << ": framebuffer incomplete, attached images must have same dimensions"
             << std::endl;
        return false;
    case GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
        std::cerr << command
             << ": framebuffer incomplete, attached images must have same format"
             << std::endl;
        return false;
    case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
        std::cerr << command
             << ": framebuffer incomplete, missing draw buffer"
             << std::endl;
        return false;
    case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
        std::cerr << command
             << ": framebuffer incomplete, missing read buffer"
             << std::endl;
        return false;
    }

    return false;
}


#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
CGLContextObj cgl_init()
{
    CGLPixelFormatAttribute attribs[] = {
        kCGLPFAPBuffer,
        kCGLPFAColorSize, (CGLPixelFormatAttribute) 32,
        (CGLPixelFormatAttribute) 0
    };
    CGLPixelFormatObj pixel_format = NULL;
    CGLContextObj cgl_context = NULL;
    long int pixel_formats = 0L;
    CGLError cgl_error;


    cgl_error = CGLChoosePixelFormat(attribs, &pixel_format, &pixel_formats);
    if (pixel_format == NULL) {
        std::cerr << command
             << ": error " << cgl_error << " occured when choosing pixel format"
             << std::endl;
    } else {
        cgl_error = CGLCreateContext(pixel_format, NULL, &cgl_context);
        if (!cgl_context) {
            std::cerr << command
                 << ": error " << cgl_error << " occured while creating a CGL context"
                 << std::endl;
        } else {
            CGLSetCurrentContext(cgl_context);
        }
    }

    return cgl_context;
}
#endif


bool initGPU(int* argcp, char** argv)
{
#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
    CGLContextObj cgl_context = cgl_init();
#else
    glutInit(argcp, argv);
    glutInitDisplayMode(GLUT_RGBA | GLUT_ALPHA);
    GlutWindowHandle = glutCreateWindow("Enblend");
#endif

    const int err = glewInit();
    if (err != GLEW_OK) {
        std::cerr << command << ": an error occured while setting up the GPU\n"
             << command << ": " << glewGetErrorString(err) << "\n"
             << command << ": \"--gpu\" flag is not going to work on this machine"
             << std::endl;
#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
        CGLDestroyContext(cgl_context);
#else
        glutDestroyWindow(GlutWindowHandle);
#endif
        exit(1);
    }

    if (Verbose >= VERBOSE_GPU_MESSAGES) {
        std::cerr << command << ": info: using graphics card: " << GLGETSTRING(GL_VENDOR) << "\n"
             << command << ": info:   renderer: " << GLGETSTRING(GL_RENDERER) << "\n"
             << command << ": info:   version: " << GLGETSTRING(GL_VERSION) << "\n";
    }

    const GLboolean has_arb_fragment_shader = glewGetExtension("GL_ARB_fragment_shader");
    const GLboolean has_arb_vertex_shader = glewGetExtension("GL_ARB_vertex_shader");
    const GLboolean has_arb_shader_objects = glewGetExtension("GL_ARB_shader_objects");
    const GLboolean has_arb_shading_language = glewGetExtension("GL_ARB_shading_language_100");
    const GLboolean has_arb_texture_float = glewGetExtension("GL_ARB_texture_float");

    if (!(has_arb_fragment_shader &&
          has_arb_vertex_shader &&
          has_arb_shader_objects &&
          has_arb_shading_language &&
          has_arb_texture_float)) {
        const char* msg[] = {"false", "true"};
        std::cerr << command << ": extension GL_ARB_fragment_shader = " << msg[has_arb_fragment_shader] << "\n"
             << command << ": extension GL_ARB_vertex_shader = " << msg[has_arb_vertex_shader] << "\n"
             << command << ": extension GL_ARB_shader_objects = " << msg[has_arb_shader_objects] << "\n"
             << command << ": extension GL_ARB_shading_language_100 = " << msg[has_arb_shading_language] << "\n"
             << command << ": extension GL_ARB_texture_float = " << msg[has_arb_texture_float] << "\n"
             << command << ": graphics card lacks the necessary extensions for \"--gpu\";" << "\n"
             << command << ": \"--gpu\" flag is not going to work on this machine" << std::endl;
#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
        CGLDestroyContext(cgl_context);
#else
        glutDestroyWindow(GlutWindowHandle);
#endif
        exit(1);
    }

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &MaxTextureSize);

    ProgramObject = glCreateProgramObjectARB();
    ShaderObject = glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
    glAttachObjectARB(ProgramObject, ShaderObject);
    glShaderSourceARB(ShaderObject, 1, &GDAKernelSource, NULL);
    glCompileShaderARB(ShaderObject);
    printInfoLog(ShaderObject);

    glLinkProgramARB(ProgramObject);
    GLint success;
    glGetObjectParameterivARB(ProgramObject, GL_OBJECT_LINK_STATUS_ARB, &success);
    if (!success) {
        std::cerr << command << ": GPU ARB shader program could not be linked\n";
        exit(1);
    }

    PiTextureParam = glGetUniformLocationARB(ProgramObject, "PiTexture");
    ETextureParam = glGetUniformLocationARB(ProgramObject, "ETexture");
    TempParam = glGetUniformLocationARB(ProgramObject, "Temperature");
    KMaxParam = glGetUniformLocationARB(ProgramObject, "KMax");

    glUseProgramObjectARB(ProgramObject);
    CHECK_GL();

    return true;
}


bool configureGPUTextures(unsigned int k, unsigned int vars)
{
    // state variables packed into vec4s
    const int width = k;
    const int height = (vars + 3) / 4;

    // http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/teximage2d.html
    if (width > 2 + GL_MAX_TEXTURE_SIZE ||
        height > 2 + GL_MAX_TEXTURE_SIZE) {
        std::cerr << command << ": texture size exceeds GPU's maximum\n";
        exit(1);
    }
    if (width % 2 != 0 || height % 2 != 0) {
        std::cerr << command << ": warning: odd texture size may be invalid for OpenGL\n";
    }

    glGenTextures(1, &PiTexture);
    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, PiTexture);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexImage2D(GL_TEXTURE_RECTANGLE_ARB, 0, GL_RGBA32F_ARB, width, height,
                 0, GL_RGBA, GL_FLOAT, NULL);
    CHECK_GL();

    glGenTextures(1, &ETexture);
    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, ETexture);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexImage2D(GL_TEXTURE_RECTANGLE_ARB, 0, GL_RGBA32F_ARB, width, height,
                 0, GL_RGBA, GL_FLOAT, NULL);
    CHECK_GL();

    glGenTextures(1, &OutTexture);
    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, OutTexture);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexImage2D(GL_TEXTURE_RECTANGLE_ARB, 0, GL_RGBA32F_ARB, width, height,
                 0, GL_RGBA, GL_FLOAT, NULL);
    CHECK_GL();

    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

    glGenFramebuffersEXT(1, &FB);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FB);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0.0, width, 0.0, height);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glViewport(0, 0, width, height);

    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_RECTANGLE_ARB,
                              OutTexture, 0);

    if (!checkFramebufferStatus()) {
        exit(1);
    }

    return true;
}


bool gpuGDAKernel(unsigned int k, unsigned int vars, double t,
                  float* packedEData, float* packedPiData, float* packedOutData)
{
    const unsigned localWidth = k;
    const unsigned localHeight = (vars + 3) / 4;

    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, PiTexture);
    glTexSubImage2D(GL_TEXTURE_RECTANGLE_ARB, 0, 0, 0, localWidth, localHeight,
                    GL_RGBA, GL_FLOAT, packedPiData);
    CHECK_GL();

    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, ETexture);
    glTexSubImage2D(GL_TEXTURE_RECTANGLE_ARB, 0, 0, 0, localWidth, localHeight,
                    GL_RGBA, GL_FLOAT, packedEData);
    CHECK_GL();

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, PiTexture);
    glUniform1iARB(PiTextureParam, 0);
    CHECK_GL();

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, ETexture);
    glUniform1iARB(ETextureParam, 1);
    CHECK_GL();

    glUniform1fARB(TempParam, t);
    CHECK_GL();
    glUniform1fARB(KMaxParam, k);
    CHECK_GL();

    glDrawBuffer(GL_COLOR_ATTACHMENT0_EXT);
    glPolygonMode(GL_FRONT, GL_FILL);
    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);                glVertex2f(0.0, 0.0);
    glTexCoord2f(localWidth, 0.0);         glVertex2f(localWidth, 0.0);
    glTexCoord2f(localWidth, localHeight); glVertex2f(localWidth, localHeight);
    glTexCoord2f(0.0, localHeight);        glVertex2f(0.0, localHeight);
    glEnd();
    CHECK_GL();

    glReadBuffer(GL_COLOR_ATTACHMENT0_EXT);
    CHECK_GL();
    glReadPixels(0, 0, localWidth, localHeight, GL_RGBA, GL_FLOAT, packedOutData);
    CHECK_GL();

    return true;
}


bool clearGPUTextures()
{
    glDeleteFramebuffersEXT(1, &FB);
    glDeleteTextures(1, &PiTexture);
    glDeleteTextures(1, &ETexture);
    glDeleteTextures(1, &OutTexture);

    return true;
}


bool wrapupGPU()
{
    if (FB != 0)
    {
        glDeleteFramebuffersEXT(1, &FB);
    }
    if (PiTexture != 0)
    {
        glDeleteTextures(1, &PiTexture);
    }
    if (ETexture != 0)
    {
        glDeleteTextures(1, &ETexture);
    }
    if (OutTexture != 0)
    {
        glDeleteTextures(1, &OutTexture);
    }

#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
    CGLContextObj cgl_context = CGLGetCurrentContext();
    if (cgl_context != NULL)
    {
        CGLDestroyContext(cgl_context);
    }
#else
    glutDestroyWindow(GlutWindowHandle);
#endif

    return true;
}

#endif // HAVE_LIBGLEW
