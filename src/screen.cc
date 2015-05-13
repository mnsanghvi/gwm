 /* Copyright (C) 1999 - 2002 Anders Holst
  *
  * This program is free software; you can redistribute it and/or
  * modify it under the terms of the GNU General Public License as
  * published by the Free Software Foundation; either version 2, or
  * (at your option) any later version.
  * 
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with this software; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  * Boston, MA 02111-1307 USA
  */


 /*********************\
 * 		       *
 *    Screen Object    *
 * 		       *
 \*********************/

#include <guile/gh.h>
#include <stdio.h>
#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "deco.hh"
#include "event.hh"
#include "fsm.hh"
#include "paint.hh"
#include "cursor.hh"
#include "screen.hh"
#include "client.hh"

/*
class ScreenContext {
public:
  ScreenContext(int screen_number);
  void InitGC();
  void Initialize();
  void Close();
  int EventHandler(XEvent* evt);
  void ReconfigureScreen(XConfigureEvent* evt);
  void SetDefaultColormap();
  void SetColormapWindow(class ClientWindow* win, Colormap cm);
  void AnnounceColormap(class ClientWindow* win, Colormap cm);
  void DenounceColormap(class ClientWindow* win);
  class ClientWindow* ColormapWindow() { return InstalledColormapCW; };
  void RegisterWindow(class ClientWindow* win);
  void UnregisterWindow(class ClientWindow* win);
    struct {
      SCM Black;	// Foreground 
      SCM White;	// Background 
    } pixel;		// screen-dependent colors 
    struct {
      GC Work;
      GC Stipple;
      GC Tile;
      GC Draw;
      GC Shape;
      GC ShapeS;
      GC ShapeT;
    } gc;	       	// the GC for the screen 
    int width, height, depth;	// dims in pixels 
    int widthMM, heightMM;		// dims in millimeters 
    Pixmap DefaultBitmap;
    int ScreenNum() { return screen; };
    class Decoration* Deco() { return rootdeco; };
    Window Root() { return root; };
    ClientWindow* First() { return first; };
    Window GwmWindow() { return gwm_window; };
protected:
    void CreateGwmLabelWindow();
    int screen;		// screen number 
    Screen *x_screen;		// X structure 
    Window root;		// root window 
    class Decoration* rootdeco;		// and its associated wob 
    int WindowCount;	// number of managed windows 
    class ClientWindow* first;
    class ClientWindow* last;
    Window gwm_window;	       // window used to set properties on 
    class ClientWindow* InstalledColormapCW;	// window of inst. colormap 
    Colormap colormap;
    Colormap InstalledColormap;
};

void SetDefaultScreen();
int ScreenOfRoot(Window window);
void SetContext(ScreenContext* scr);

*/

extern FILE* gwmlog;   // log file used for debugging

#define GWMLOG(str, obj) if(gwmlog) { fprintf(gwmlog, str, obj); fflush(gwmlog); }

ScreenContext::ScreenContext(int screen_number)
{
  XSetWindowAttributes wa;
  screen = screen_number;
  x_screen = ScreenOfDisplay(dpy, screen_number);
  width = DisplayWidth(dpy, screen_number);
  height = DisplayHeight(dpy, screen_number);
  depth = DisplayPlanes(dpy, screen_number);
  widthMM = DisplayWidthMM(dpy, screen_number);
  heightMM = DisplayHeightMM(dpy, screen_number);
    
  root = RootWindow(dpy, screen_number);
  WindowCount = 0;
  first = 0;
  last = 0;
  /* init root window colormap */
  wa.colormap = colormap = DefaultColormap(dpy, screen);
  XChangeWindowAttributes(dpy, root, CWColormap, &wa);
  XInstallColormap(dpy, wa.colormap);
  InstalledColormapCW = 0;
  InstalledColormap = 0;
  InitGC();
  rootdeco = new UScreen(this);
}

/*
 * Initialize GCs used 
 */
void ScreenContext::InitGC()
{
  XGCValues       gc_values;

  DefaultBitmap = MakeDefaultBitmap(root);
  pixel.Black = color2scm(new WlColor("black", BlackPixel(dpy, screen)));
  pixel.White = color2scm(new WlColor("white", WhitePixel(dpy, screen)));
  scm_gc_protect_object(pixel.Black);
  scm_gc_protect_object(pixel.White);

  gc_values.graphics_exposures = False;
  gc_values.foreground = BlackPixel(dpy, screen);
  gc_values.background = WhitePixel(dpy, screen);
  gc_values.line_width = 1;
  gc_values.cap_style = CapProjecting;
  gc_values.ts_x_origin = 0;
  gc_values.ts_y_origin = 0;
  gc_values.function = GXcopy;
  gc.Work = XCreateGC(dpy, root,
                      GCGraphicsExposures | GCForeground |
                      GCBackground | GCLineWidth | GCCapStyle | GCFunction,
                      &gc_values);
  gc_values.fill_style = FillStippled;
  gc.Stipple = XCreateGC(dpy, root,
                         GCFunction | GCTileStipXOrigin | GCGraphicsExposures
                         | GCTileStipYOrigin | GCLineWidth | GCCapStyle | GCFillStyle,
                         &gc_values);
  gc_values.fill_style = FillTiled;
  gc.Tile = XCreateGC(dpy, root,
                      GCFunction | GCTileStipXOrigin | GCGraphicsExposures
                      | GCTileStipYOrigin | GCLineWidth | GCCapStyle | GCFillStyle,
                      &gc_values);
  gc_values.foreground = 1;
  gc_values.background = 0;
  gc.Shape = XCreateGC(dpy, DefaultBitmap,
                       GCGraphicsExposures | GCForeground |
                       GCBackground | GCLineWidth | GCCapStyle | GCFunction,
                       &gc_values);
  gc_values.fill_style = FillStippled;
  gc.ShapeS = XCreateGC(dpy, DefaultBitmap,
                        GCFunction | GCTileStipXOrigin | GCGraphicsExposures
                        | GCTileStipYOrigin | GCLineWidth | GCCapStyle | GCFillStyle,
                        &gc_values);
  gc_values.fill_style = FillTiled;
  gc.ShapeT = XCreateGC(dpy, DefaultBitmap,
                        GCFunction | GCTileStipXOrigin | GCGraphicsExposures
                        | GCTileStipYOrigin | GCLineWidth | GCCapStyle | GCFillStyle,
                        &gc_values);
  gc_values.line_width = 0;
  gc_values.foreground = 0xfd;
  gc_values.function = GXxor;
  gc_values.subwindow_mode = IncludeInferiors;
  gc.Draw = XCreateGC(dpy, root,
                      GCGraphicsExposures | GCLineWidth | GCForeground
                      | GCFunction | GCSubwindowMode,
                      &gc_values);
}

/* create an unmapped window whose ID is stored on the root window of each
 * managed screen in the GWM_RUNNING property
 * current screen is given in the context
 */
void ScreenContext::CreateGwmLabelWindow()
{
  XSetWindowAttributes wa;
  wa.override_redirect = True;
  gwm_window = XCreateWindow (dpy, root, 
                              -100, -100, 10, 10, 0, 0,
                              InputOnly, CopyFromParent,
                              CWOverrideRedirect,
                              &wa);
  XChangeProperty(dpy, root, XA_GWM_RUNNING, XA_GWM_RUNNING,
                  32, PropModeReplace, (unsigned char *) &gwm_window, 1);
  XChangeProperty(dpy, gwm_window, XA_GWM_RUNNING, XA_GWM_RUNNING,
                  32, PropModeReplace, (unsigned char *) &gwm_window, 1);
}

int Mapped(Window w)
{
  XWindowAttributes wa;
  if (TrapXErrors(XGetWindowAttributes(dpy, w, &wa)))
    return 0;
  else
    return (wa.map_state != IsUnmapped);
}

/* initialize screen AFTER user profile
 */
void ScreenContext::Initialize()
{
  SCM scr;
  unsigned int i;
  ClientWindow* cw;
  unsigned int nb_windows;
  Window *windows, dummywin, parent;
  if (SCM_VARIABLE_REF(v_describe_screen) == SCM_UNDEFINED) {
    if (!GWM_WidgetMode)
      gwm_warning("\"describe-screen\" is undefined, using empty decoration",
                  0);
  } else {
    SetContext(this);
    scr = gwm_apply1_catch(SCM_VARIABLE_REF(v_describe_screen), rootdeco->scm());
    if (!WLDECOP(scr) || WL_DECO(scr) != rootdeco) {
      gwm_warning("the return value of \"describe-screen\": ~S\n    is not a screen decoration, using empty decoration",
                  scr);
    }
  }
  rootdeco->open(ScreenStatus, 0, this);
  if (!GWM_WidgetMode) {
    CreateGwmLabelWindow();
    if (XQueryTree(dpy, root, &dummywin, &parent, &windows, &nb_windows)) {
      for (i=0; i<nb_windows; i++) {
        if ((cw = LookUpClient(windows[i])))
          cw->InitialMap();
        else if (Mapped(windows[i]) || GWM_Mapall) 
          DecorateWindow(windows[i], this, 0, 0);
      }
      if (windows)
        XFree(windows);
    }
  }
  rootdeco->execopen();
}

/* Here we clean up everything for the next WM
 */
void ScreenContext::Close()
{
  unsigned int i;
  Decoration* wob;
  unsigned int nb_windows;
  Window *windows, dummywin, parent;
  ProcessGwmEvents();
  rootdeco->execclose();
  if (WindowCount) {
    XQueryTree(dpy, root, &dummywin, &parent, &windows, &nb_windows);
    for (i=0; i<nb_windows; i++)
      if ((wob = LookUpDeco(windows[i]))
          && !wob->Parent() && (wob->Type() & WindowStatus) && wob->Win()) {
        wob->Win()->UnDecorateWindow(1);
      }
    if (windows)
      XFree(windows);
  }
  rootdeco->close(0);
}

/* Called when an event is reported to rootWindow.
 * There we detect and decorate newly created windows on first map
 */
int ScreenContext::EventHandler(XEvent* evt)
{
    ClientWindow* cw;
    Decoration* dec;

    switch (evt->type) {

    case MapRequest:			/* look if this window is not yet
					 * managed to decorate it */
	if (evt->xmaprequest.parent == root)
	    if ((dec = LookUpDeco(evt->xmaprequest.window))) {
              if (!dec->Win())       // does this ever happen ? *****
		    XMapWindow(dpy, evt->xmaprequest.window);
	    } else {
		if ((cw = LookUpClient(evt->xmaprequest.window))) {
                    cw->InitialMap();
		} else {
                    DecorateWindow(evt->xmaprequest.window, this, 0, 1);
		}
	    }
        return 1;

    case ColormapNotify:		/* the screen colormap has changed:
					 * reflect it if the current wob has
					 * no defined colormap */
	if (evt->xcolormap.c_new) {	/* colormap has changed */
	    Colormap cm = (evt->xcolormap.colormap == None ?
                           DefaultColormap(dpy, screen) :
                           evt->xcolormap.colormap);
            AnnounceColormap(0, cm);
	}
	return 1;

    case UnmapNotify:{			/* iccc: to withdraw a window */

	    if (evt->xunmap.send_event
		&& evt->xunmap.from_configure == False
		&& (cw = LookUpClient(evt->xunmap.window))) {
              cw->UnDecorateWindow(0);
	    }
	}
	return 0;

    case ConfigureRequest:{		/* unmapped window */

	    /* warning: window may have been redecorated meanwhile */
	    if ((cw = LookUpClient(evt->xconfigurerequest.window))) {
		cw->ConfigureRequestEventHandler((XConfigureRequestEvent*) evt);	/* was remapped */
	    } else if (evt->xconfigurerequest.window != root) {
		ConfigureUnmappedWindow((XConfigureRequestEvent*) evt);	/* not yet known, obey */
	    }
	}
	return 0;

    case ConfigureNotify:{		/* screen size change */
	    if (evt->xconfigure.window == root) {
//                printf("Screen size changed to (%d, %d)\n", ((XConfigureEvent*) evt)->width, ((XConfigureEvent*) evt)->height);
		ReconfigureScreen((XConfigureEvent*) evt);
	    }
	}
	return 0;

    default:
	return 0;
    }
}

void ScreenContext::ReconfigureScreen(XConfigureEvent* evt)
{
  if (width != evt->width || height != evt->height) {
    width = evt->width; //DisplayWidth(dpy, screen);
    height = evt->height; //DisplayHeight(dpy, screen);
    //depth = DisplayPlanes(dpy, screen);
    //widthMM = DisplayWidthMM(dpy, screen);
    //heightMM = DisplayHeightMM(dpy, screen);
//    printf("  evt: %d x %d,   scr: %d x %d\n", evt->width, evt->height, width, height);
    ((UScreen*)rootdeco)->ResizeScreen(width, height);
  }
}

void ScreenContext::SetDefaultColormap()
{
  InstalledColormapCW = 0;
  if (InstalledColormap && InstalledColormap != colormap)
    XInstallColormap(dpy, colormap);
  InstalledColormap = 0;
}

void ScreenContext::SetColormapWindow(ClientWindow* win, Colormap cm)
{
  InstalledColormapCW = win;
  if (cm) {
    InstalledColormap = cm;
    XInstallColormap(dpy, cm);
  } else if (InstalledColormap) {
    if (InstalledColormap != colormap)
      XInstallColormap(dpy, colormap);
    InstalledColormap = 0;
  }
}

void ScreenContext::AnnounceColormap(ClientWindow* win, Colormap cm)
{
  if (win == 0) {
    if (InstalledColormap == 0 && colormap != cm)
      XInstallColormap(dpy, cm);
    colormap = cm;
  } else if (InstalledColormapCW == win) {
    if (cm) {
      InstalledColormap = cm;
      XInstallColormap(dpy, cm);
    } else if (InstalledColormap) {
      if (InstalledColormap != colormap)
        XInstallColormap(dpy, colormap);
      InstalledColormap = 0;
    }
  }
}

void ScreenContext::DenounceColormap(ClientWindow* win)
{
  if (InstalledColormapCW == win) {
    InstalledColormapCW = 0;
    if (InstalledColormap && InstalledColormap != colormap)
      XInstallColormap(dpy, colormap);
    InstalledColormap = 0; 
  }
}

void ScreenContext::RegisterWindow(ClientWindow* win)
{
  if (first)
    first->previous = win;
  else
    last = win;
  win->next = first;
  first = win;
  win->previous = 0;
  WindowCount++;
}

void ScreenContext::UnregisterWindow(ClientWindow* win)
{
  if (first == win)
    first = win->next;
  else if (win->previous)
    win->previous->next = win->next;
  if (last == win)
    last = win->previous;
  else if (win->next)
    win->next->previous = win->previous;
  win->previous = win->next = 0;
}

/* the "current screen" functions 
 */

void SetContext(ScreenContext* scr)
{
  Context = scr;
}

SCM_REGISTER_PROC(s_current_screen, "screen", 0, 0, 0, root_window);
SCM_DEFINE(root_window, "root-window", 0, 0, 0,
           (),
           "Get the current root window.")
{
  return Context->Deco()->scm();
}

SCM_REGISTER_PROC(s_current_screen_set, "set-screen!", 1, 0, 0, root_window_set);
SCM_DEFINE(root_window_set, "set-root-window!", 1, 0, 0,
           (SCM screen),
           "Set the current root window to screen (or if a window is given, to its screen).")
{
  if (!WLSCREENP(screen))
    gwm_wrong_type_arg(s_current_screen_set, 1, screen, "screen");
  SetContext(WL_SCREEN(screen));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(root_deco, "root-deco", 1, 0, 0,
           (SCM deco),
           "Get screen of deco.")
{
  must_be_valid_deco(s_root_deco, deco, 1);
  return WL_DECO(deco)->Screen()->Deco()->scm();
}

SCM_DEFINE(index_screen, "index->screen", 1, 0, 0,
           (SCM index),
           "Get the screen object from its index.")
{
  int ind;
  must_be_number(s_index_screen, index, 1);
  ind = gh_scm2int(index);
  if (ind >= 0 && ind < GWM_ScreenCount && GWMManagedScreens[ind]) {
    return GWMManagedScreens[ind]->Deco()->scm();
  } else {
    return SCM_BOOL_F;
  }
}

SCM_DEFINE(screen_index, "screen-index", 0, 1, 0,
           (SCM screen),
           "Get the index of the screen (the current screen if no argument).")
{
  if (screen == SCM_UNDEFINED)
    return gh_int2scm(Context->ScreenNum());
  else if (WLSCREENP(screen))
    return gh_int2scm(WL_SCREEN(screen)->ScreenNum());
  else
    gwm_wrong_type_arg(s_screen_index, 1, screen, "screen");
  return SCM_UNSPECIFIED; // Not reached
}

SCM_DEFINE(screen_width, "screen-width", 0, 1, 0,
           (SCM screen),
           "Get the width of the screen (current or given) in pixels.")
{
  if (screen == SCM_UNDEFINED)
    return gh_int2scm(Context->width);
  else if (WLSCREENP(screen))
    return gh_int2scm(WL_SCREEN(screen)->width);
  else
    gwm_wrong_type_arg(s_screen_width, 1, screen, "screen");
  return SCM_UNSPECIFIED; // Not reached
}

SCM_DEFINE(screen_height, "screen-height", 0, 1, 0,
           (SCM screen),
           "Get the height of the screen (current or given) in pixels.")
{
  if (screen == SCM_UNDEFINED)
    return gh_int2scm(Context->height);
  else if (WLSCREENP(screen))
    return gh_int2scm(WL_SCREEN(screen)->height);
  else
    gwm_wrong_type_arg(s_screen_height, 1, screen, "screen");
  return SCM_UNSPECIFIED; // Not reached
}

SCM_DEFINE(screen_depth, "screen-depth", 0, 1, 0,
           (SCM screen),
           "Get the depth of the screen (current or given).")
{
  if (screen == SCM_UNDEFINED)
    return gh_int2scm(Context->depth);
  else if (WLSCREENP(screen))
    return gh_int2scm(WL_SCREEN(screen)->depth);
  else
    gwm_wrong_type_arg(s_screen_depth, 1, screen, "screen");
  return SCM_UNSPECIFIED; // Not reached
}

SCM_DEFINE(screen_widthMM, "screen-widthMM", 0, 1, 0,
           (SCM screen),
           "Get the width of the screen (current or given) in millimeters.")
{
  if (screen == SCM_UNDEFINED)
    return gh_int2scm(Context->widthMM);
  else if (WLSCREENP(screen))
    return gh_int2scm(WL_SCREEN(screen)->widthMM);
  else
    gwm_wrong_type_arg(s_screen_widthMM, 1, screen, "screen");
  return SCM_UNSPECIFIED; // Not reached
}

SCM_DEFINE(screen_heightMM, "screen-heightMM", 0, 1, 0,
           (SCM screen),
           "Get the heigth of the screen (current or given) in millimeters.")
{
  if (screen == SCM_UNDEFINED)
    return gh_int2scm(Context->heightMM);
  else if (WLSCREENP(screen))
    return gh_int2scm(WL_SCREEN(screen)->heightMM);
  else
    gwm_wrong_type_arg(s_screen_heightMM, 1, screen, "screen");
  return SCM_UNSPECIFIED; // Not reached
}

SCM_DEFINE(get_visual_type, "screen-type", 0, 1, 0,
           (SCM screen),
           "Get the visual type of the screen (current or given): mono, gray, or color.")
{
  ScreenContext* scrn;
  if (screen == SCM_UNDEFINED)
    scrn = Context;
  else if (WLSCREENP(screen))
    scrn = WL_SCREEN(screen);
  else
    gwm_wrong_type_arg(s_get_visual_type, 1, screen, "screen");
  switch (DefaultVisual(dpy, scrn->ScreenNum())->c_class) {
  case StaticGray:
  case GrayScale:
    if (WL_SCREEN(screen)->depth > 1)
      return WA_gray;
    else
      return WA_mono;
  default:
    return WA_color;
  }
}

SCM_DEFINE(screen_count, "screen-count", 0, 0, 0,
           (),
           "Get the number of managed screens.")
{
  return gh_int2scm(GWM_ScreenCount);
}

SCM_DEFINE(get_display_name, "display-name", 0, 0, 0,
           (),
           "Get the name of the default screen.")
{
  return scm_makfrom0str(GWM_DisplayName);
}

/* sets Context to DefaultScreen, or first one... */
void SetDefaultScreen()
{
    ScreenContext** ContextPtr;
    if (GWMManagedScreens[DefaultScreen(dpy)]) {
        Context = GWMManagedScreens[DefaultScreen(dpy)];
    } else {
	for (ContextPtr = GWMManagedScreens;; ContextPtr++)
	    if (*ContextPtr) {
                Context = (*ContextPtr);
		break;
	    }
    }
}

int ScreenOfRoot(Window window)
{
    int             screen;

    for (screen = 0; screen < GWM_ScreenCount; screen++)
	if (RootWindow(dpy, screen) == window)
	    return screen;
    return -1;
}

void init_scm_screen()
{
#include "screen.x"
}
