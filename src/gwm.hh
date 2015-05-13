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


/***************************************************************\
* 							        *
* 	general include file, contains GWM specific typedefs    *
* 							        *
\***************************************************************/

#include "gwmconfig.h"

/* some parameters */

#define GWM_VERSION "GWM 2.1 release 1"

#define WLPROFILE_USER_VARIABLE "GWMPROFILE"
#define USER_PROFILE "gwm.scm"
#define GWMPATH_SHELL_VARIABLE "GWMPATH"

/*
 *  Maximum size for:
 *      length of name of CACHED objets (fonts, etc...)
 * 	file names (path look-up)
 * 	X properties (only "machine name" for now)
 *
 *  Should alway be >= 256, 1024 is fine.
 *  Used only for speed concerns to allocate temp strings in the stack
 */

#define MAX_TEMP_STRING_SIZE 1024

/* shell variables */

#define DEFAULT_FONT "fixed"		/* MUST exist! */

#ifndef NULL
#define NULL 0
#endif /* NULL */

#ifdef NEED_GUILE_FUNCTION_CAST
/* compensate for bug in guile-1.4 */
#undef SCM_FUNC_CAST_ARBITRARY_ARGS
#define SCM_FUNC_CAST_ARBITRARY_ARGS SCM (*)()
#endif /* NEED_GUILE_FUNCTION_CAST */

#ifdef CARD32
typedef CARD32 Card32;
#else /* CARD32 */
typedef unsigned long	Card32;
#endif /* CARD32 */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h> 
#endif
#ifdef HAVE_CTYPE_H
#include <ctype.h> 
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h> 
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef SIGCLD
#define SIGCLD SIGCHLD
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#else
#include <strings.h>
#endif

#ifdef HAVE_STRCHR
#ifdef MISSING_STRCHR_DECL
extern "C" char *strchr(const char *s, int c);
#endif
#elif defined HAVE_INDEX
#define strchr(string, char) index(string, char)
#endif /* HAS_STRCHR */

#ifndef HAVE_BZERO
#define bzero(dest, count) memset(dest, 0, count)
#elif defined MISSING_BZERO_DECL
extern "C" void bzero(void *s, size_t n);
#endif
#ifndef HAVE_BCOPY
#define bcopy(source, dest, count) memcpy(dest, source, count)
#elif defined MISSING_BCOPY_DECL
extern "C" void bcopy (const void *src, void *dest, size_t n);
#endif

#if defined(HAVE_GETHOSTNAME) && defined(MISSING_GETHOSTNAME_DECL)
extern "C" int gethostname(char *name, size_t len);
#endif
#if defined(MISSING_SLEEP_DECL)
extern "C" unsigned int sleep(unsigned int seconds);
#endif
#if defined(HAVE_USLEEP) && defined(MISSING_USLEEP_DECL)
extern "C" void usleep(unsigned long usec);
#endif
#if defined(HAVE_WAIT3) && defined(MISSING_WAIT3_DECL)
extern "C" pid_t wait3(int *status, int options, struct rusage *rusage);
#endif
#if defined(HAVE_FTIME) && defined(MISSING_FTIME_DECL)
extern "C" int ftime(struct timeb *tp);
#endif

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/xpm.h>
#include <X11/Xresource.h>
#include <X11/extensions/shape.h>

inline int Max(int x, int y) { return (x<y ? y : x ); };
inline int Min(int x, int y) { return (x<y ? x : y ); };
inline int Round(float v) { int vv=(int)(v+0.5); return (v+0.5-vv<0.0 ? vv-1 : vv); };

/* customization flags */

#ifndef TRUE
#define TRUE			1
#define FALSE			0
#endif

/* X types */

#define LegalPointerEventMask(mask) ((mask) & (0xffff ^ 0x8003))

#define GWMNumEvents 22

#define GWMMapEvent (LASTEvent+1)
#define GWMUnmapEvent (LASTEvent+2)
#define GWMResizeEvent (LASTEvent+3)
#define GWMMoveEvent (LASTEvent+4)
#define GWMStackEvent (LASTEvent+5)
#define GWMUserEvent (LASTEvent+6)
#define GWMEnterEvent (LASTEvent+7)
#define GWMLeaveEvent (LASTEvent+8)
#define GWMFocusinEvent (LASTEvent+9)
#define GWMFocusoutEvent (LASTEvent+10)
#define GWMOpeningEvent (LASTEvent+11)
#define GWMClosingEvent (LASTEvent+12)
#define GWMExposeEvent (LASTEvent+13)
#define GWMExposeBordEvent (LASTEvent+14)

#define GWMMapMask 1
#define GWMUnmapMask 2
#define GWMResizeMask 4
#define GWMMoveMask 8
#define GWMStackMask 16
#define GWMUserMask 32
#define GWMEnterMask 64 
#define GWMLeaveMask 128 
#define GWMFocusinMask 256 
#define GWMFocusoutMask 512 
#define GWMOpeningMask 1024 
#define GWMClosingMask 2048 


/*
 * ICCC structures
 */

#define WM_STATE_Withdrawn 0
#define WM_STATE_Normal 1
#define WM_STATE_Iconified 3

typedef struct _WM_STATE_PROP {
    Card32	state;		/* WM_STATE_Withdrawn, _Normal, _Iconified */
    Window	icon;		/* X window id of the icon */
} *WM_STATE_PROP;

#define WM_STATE_PROP_Length 2  /* in Card32 elements */

/* max length of some properties */

#define WM_COLORMAP_WINDOWS_PROP_Length 80 /* number of sub-wins colormaps */
#define WM_PROTOCOLS_PROP_Length        40 /* number of protocols */

#define ANY -1

/* The Context is a structure of all screen-dependent variables
 * Multi-screen operation will be handled by switching contextes
 */

extern class ScreenContext** GWMManagedScreens;/* the list of all screens on the
					   display, being NULL if not
					   managed. */
    
extern class ScreenContext* Context;		/* the current screen (pointer to) */

#define FOR_ALL_SCREENS(screen) \
  for (int _i_=0; _i_<GWM_ScreenCount; _i_++) \
    if ((screen = GWMManagedScreens[_i_]))

/*
 * GWM global variables, common to the display
 */

extern Display    *dpy;		/* THE display */
extern Time 	GWMTime;/* the latest known server date */
extern XContext 	deco_context;	/* to retrieve wobs via hooks */
extern XContext 	client_context;	/* to retrieve wobs via clients */
extern int		GWM_ShapeExtension; /* display has shape extension ? */
extern int		GWM_ShapeEventBase; /* first event # of shape ext. */
extern int		GWM_ShapeErrorBase; /* first error # of shape ext. */

/* switches */
extern int GWM_Mapall;
extern int GWM_FreezeServer;
extern int GWM_Quiet;
extern int GWM_Synchronize;
extern char* GWM_DisplayName;
extern int GWM_ScreenCount;       
extern int GWM_Monoscreen;
extern int GWM_No_set_focus;
extern char* GWM_ScreensNotManaged;
extern int GWM_WidgetMode;
extern int GWM_kill_pid;
extern int GWM_kill_pid_sig;
extern char* GWM_path;
extern SCM GWM_host_name;

/* static data for remebering GWM state */
extern int GWM_argc;			/* unix argc argv of GWM */
extern char ** GWM_argv;
extern char ** GWM_restart_argv;
extern unsigned int GWM_new_window_id;
extern int GWM_is_starting;
extern int GWM_is_ending;
extern int GWM_time_of_last_bus_error;
extern int GWM_processing_frozen;
extern int GWM_rubber_feedback;

/* masks */
extern unsigned int ClientMask;
extern unsigned int ClientClientMask;
extern unsigned int RootMask;
extern unsigned int RootMask2;
extern unsigned int MenuMask;
extern unsigned int WobMask;

/* X Atoms */
extern Atom	XA_WM_STATE;
extern Atom	XA_WM_COLORMAP_WINDOWS;
extern Atom	XA_WM_CHANGE_STATE;
extern Atom	XA_WM_PROTOCOLS;
extern Atom	XA_WM_TAKE_FOCUS;
extern Atom	XA_WM_SAVE_YOURSELF;
extern Atom	XA_WM_DELETE_WINDOW;
extern Atom	XA_GNOME_SUPPORTING;
extern Atom	XA_GNOME_PROTOCOLS;
extern Atom     XA_GWM_RUNNING;

/* SCM Symbols */

extern SCM v_gwm_quiet;
extern SCM v_double_click_delay;
extern SCM v_relevant_modifiers;

extern SCM v_describe_window;
extern SCM v_describe_icon;
extern SCM v_describe_screen;
extern SCM v_move_window;
extern SCM v_resize_window;
extern SCM v_move_resize_window;
extern SCM v_raise_window;
extern SCM v_lower_window;
extern SCM v_iconify_window;
extern SCM v_de_iconify_window;
extern SCM v_load_path;

extern SCM k_direction;
extern SCM k_min_width;
extern SCM k_max_width;
extern SCM k_min_height;
extern SCM k_max_height;
extern SCM k_separator;
extern SCM k_margin;
extern SCM k_borderwidth;
extern SCM k_bordercolor;
extern SCM k_background;
extern SCM k_foreground;
extern SCM k_behavior;
extern SCM k_anchor;
extern SCM k_gravity;
extern SCM k_cursor;
extern SCM k_property;
extern SCM k_font;
extern SCM k_horizontal_margin;
extern SCM k_vertical_margin;
extern SCM k_xpm_closeness;
extern SCM k_shape;
extern SCM k_angle;
extern SCM k_mirrored;
extern SCM k_rotate;
extern SCM k_crop;
extern SCM k_color;
extern SCM k_width;
extern SCM k_height;
extern SCM k_context;
extern SCM k_name;
extern SCM k_icon_name;
extern SCM k_class_name;
extern SCM k_client_name;
extern SCM k_decoration;
extern SCM k_icon_decoration;
extern SCM k_no_freeze;
extern SCM k_grab_keyboard;
extern SCM k_grab_children;
extern SCM k_confine_pointer;
extern SCM k_menu_parent;
extern SCM k_steal;
extern SCM k_resend;
extern SCM k_propagate;
extern SCM k_cached;
extern SCM k_invert_color;

extern SCM WA_icon;
extern SCM WA_window;
extern SCM WA_mapped;
extern SCM WA_stacking_order;
extern SCM WA_all_screens;
extern SCM WA_mono;
extern SCM WA_gray;
extern SCM WA_color;
extern SCM WA_transparent;
extern SCM WA_hole;
extern SCM WA_x;
extern SCM WA_y;
extern SCM WA_static;
extern SCM WA_forget;
extern SCM WA_northwest;
extern SCM WA_north;
extern SCM WA_northeast;
extern SCM WA_west;
extern SCM WA_center;
extern SCM WA_east;
extern SCM WA_southwest;
extern SCM WA_south;
extern SCM WA_southeast;
extern SCM WA_horizontal;
extern SCM WA_vertical;
extern SCM WA_on_event;
extern SCM WA_event;
extern SCM WA_deco;

extern SCM DefaultClientClass;
extern SCM DefaultClientName;
extern SCM DefaultWindowName;
extern SCM DefaultMachineName;
extern SCM DefaultIconName;
extern SCM DefaultFont;

class ClientWindow* LookUpClient(Window window);
class IClient* LookUpInnerClient(Window window);
class Decoration* LookUpDeco(Window window);
class ScreenContext* ContextOfXWindow(Window window);
void GWM_ProcessEvents(int blocking);
void GWM_InternSleep(int rest);
SCM GWM_AwaitPointerEvent(int rest);
int gwm_proper_time();
void gwm_end(int n);
void GWM_re_init_PointerRoot();
