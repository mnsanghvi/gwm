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


/*************************************************************\
* 							      *
* Guile/GWM interface.					      *
* 							      *
* Here are defined Guile objects referring to X objects	      *
* To add a function:					      *
*        - declare it (coded in or extern)		      *
*        - add it to the declaration of predefined functions  *
*          in the gwm_init function			      *
* 							      *
\*************************************************************/

/*  include  */

#include <guile/gh.h>
#include <stdio.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "deco.hh"
#include "event.hh"
#include "fsm.hh"
#include "font.hh"
#include "active.hh"
#include "paint.hh"
#include "cursor.hh"
#include "screen.hh"
#include "client.hh"
#include "drawing.hh"

#ifdef HAVE_READLINE
extern "C" {
#include <readline/readline.h>
#ifdef MISSING_RL_DEPREP_TERMINAL_DECL
void rl_deprep_terminal(void);
#endif
}
#endif

extern void init_scm_error();
extern void init_scm_gwm();
extern void init_scm_deco();
extern void init_scm_screen();
extern void init_scm_client();
extern void init_scm_cursor();
extern void init_scm_drawing();
extern void init_scm_event();
extern void init_scm_fsm();
extern void init_scm_font();
extern void init_scm_active();
extern void init_scm_paint();
extern void init_scm_feedback();
extern void init_scm_decofunc();
extern void init_scm_gwmfunc();
extern void init_scm_wops();

extern char * getenv();

/*  local defs  */

int wl_getbool(SCM obj, const char* subr)
{
  if (scm_is_integer(obj))
    return (gh_scm2int(obj) != 0);
  else if (obj == SCM_BOOL_T)
    return 1;
  else if (obj == SCM_BOOL_F)
    return 0;
  else 
    gwm_wrong_type_arg(subr, 0, obj, "boolean");
  return 0;
}

int wl_getint(SCM obj, const char* subr)
{
  if (scm_is_integer(obj))
    return gh_scm2int(obj);
  else 
    gwm_wrong_type_arg(subr, 0, obj, "number");
  return 0;
}

int wl_getposint(SCM obj, const char* subr, int n)
{
  if (obj == SCM_BOOL_F)
    return -1;
  else if (scm_is_integer(obj) && scm_to_int(obj) >= 0)
    return gh_scm2int(obj);
  else 
    gwm_wrong_type_arg(subr, n, obj, "positive integer");
  return -1;
}

char* wl_getstring(SCM obj, const char* subr, int n)
{
  char* str;
  int slen;
  if (gh_string_p(obj)) {
    slen = scm_i_string_length(obj);
    str = strncpy(new char[slen+1], scm_i_string_chars(obj), slen);
    str[slen] = 0;
    return str;
  } else
    gwm_wrong_type_arg(subr, n, obj, "string");
  return 0;
}

char* wl_getsymbol(SCM obj, const char* subr, int n)
{
  char* str;
  int slen;
  if (gh_symbol_p(obj)) {
    slen = scm_i_symbol_length(obj);
    str = strncpy(new char[slen+1], scm_i_symbol_chars(obj), slen);
    str[slen] = 0;
    return str;
  } else
    gwm_wrong_type_arg(subr, n, obj, "symbol");
  return 0;
}

void must_be_number(const char* subr, SCM obj, int n)
{
    if (!scm_is_integer(obj))
      gwm_wrong_type_arg(subr, n, obj, "number");
}

void must_be_string(const char* subr, SCM obj, int n)
{
    if (!gh_string_p(obj))
      gwm_wrong_type_arg(subr, n, obj, "string");
}

void must_be_deco(const char* subr, SCM obj, int n)
{
    if (!WLDECOP(obj))
      gwm_wrong_type_arg(subr, n, obj, "deco");
}

void must_be_valid_deco(const char* subr, SCM obj, int n)
{
    if (!(WLDECOP(obj) && WL_DECO(obj)->Valid() > 0))
      gwm_wrong_type_arg(subr, n, obj, "valid deco");
}

void must_be_window(const char* subr, SCM obj, int n)
{
    if (!WLWINDOWP(obj))
      gwm_wrong_type_arg(subr, n, obj, "window");
}

void must_be_valid_window(const char* subr, SCM obj, int n)
{
    if (!(WLWINDOWP(obj) && WL_DECO(obj)->Valid() > 0))
      gwm_wrong_type_arg(subr, n, obj, "opened window");
}

/* When not-yet mapped configures its window, we must execute the 
 * configure request!
 */
void ConfigureUnmappedWindow(XConfigureRequestEvent* evt)
{
  XWindowChanges	window_changes;
  window_changes.x = evt->x;
  window_changes.y = evt->y;
  window_changes.width = evt->width;
  window_changes.height = evt->height;
  window_changes.border_width = evt->border_width;
  window_changes.sibling = evt->above;
  window_changes.stack_mode = evt->detail;
  TrapXErrors(XConfigureWindow(dpy,
                               evt->window, evt->value_mask,
                               &window_changes));
}

/* FindToplevelWindow
 * given a window, move back in hierarchy to return the toplevel one
 * If given root, returns 0
 */
Window FindToplevelWindow(Window w, ScreenContext* scr)
{
  Window parent = 0;
  Window* children;		/* list of root sons */
  unsigned int nchildren;	/* number of children */
  Window root;
  if (w == scr->Root())
    return 0;
  for (;;) {
    if(!XQueryTree(dpy, w, &root, &parent, &children, &nchildren))
      return 0;			/* failed */
    if (children) XFree(children);
    if (parent == scr->Root())
      return w;
    w = parent;
  }
}

/* IsAToplevelWindow
 * Examine a non-gwm decorated window to see if it is legal to decorate it
 */

int IsAToplevelWindow(Window w, ScreenContext* scr)
{
  Window dummywin, parent;	/* dummy parent */
  Window* children;		/* list of root sons */
  unsigned int nchildren;	/* number of children */
  if (XQueryTree(dpy, w, &dummywin, &parent, &children, &nchildren)) {
    if (children) XFree(children);
    if (parent == scr->Root()) /* w is son of root, OK */
      return 1;
  }					/* then not valid! */
  return 0;
}

/* ClientWindowAncestorOfWindow
 * looks into all the ancestors to eventually find a ClientWindow, which would
 * be returned, or 0
 */

ClientWindow* ClientWindowAncestorOfWindow(Window w, ScreenContext* scr)
{
  Window toplevel = FindToplevelWindow(w, scr);
  Decoration* deco;
  if (toplevel
      && (deco = LookUpDeco(toplevel))) {
    return deco->Win();
  } else {
    return 0;
  }
}

/* SemiDecoratedWindow
 * given a X window, returns either the GWM window associated to it,
 * or the X window Id as a number if the window is unknown to GWM
 * used to look for window group leaders and transient_for masters
 * returns #f if fails
 */

SCM SemiDecoratedWindow(Window w)
{
  ClientWindow* cw;
  ScreenContext* scr = ContextOfXWindow(w);
  if (!scr)
    return SCM_BOOL_F;
  if ((cw = LookUpClient(w))) {
    return cw->Deco()->scm();
  } else if (LookUpDeco(w)) {
    return SCM_BOOL_F;   /* Or should we use the client of the decoration ? */
  } else if ((cw = ClientWindowAncestorOfWindow(w, scr))) {
    return cw->Client()->scm();	    
  } else if (IsAToplevelWindow(w, scr)) {
    return gh_int2scm(w);
  } else
    return SCM_BOOL_F;
}

// The following two function taken from goops - remove them if multiply defined
SCM gwm_get_keyword (SCM key, SCM l, int len, SCM default_value)
{
  int i;
  for (i = 0; i < len; i += 2) {
    if (SCM_CAR (l) == key)
      return SCM_CADR (l);
    l = SCM_CDDR (l);
  }
  return default_value;
}

SCM_DEFINE(get_keyword, "get-keyword", 3, 0, 0,
           (SCM key, SCM l, SCM default_value),
           "")
{
  int len, i;
  SCM l2, ret;
  if (!SCM_NIMP(key) || !scm_is_keyword(key))
    gwm_misc_error(s_get_keyword, "Bad keyword: ~S", key);
  len = scm_ilength (l);
  if (len < 0 || (len & 1))
    gwm_misc_error(s_get_keyword, "Bad keyword-value list: ~S", l);
  for (i = 0, l2 = l; i < len; i += 2) {
    if (!(SCM_NIMP(SCM_CAR(l2)) && scm_is_keyword(SCM_CAR(l2))))
      gwm_misc_error(s_get_keyword, "Bad keyword: ~S", SCM_CAR(l2));
    l2 = SCM_CDDR (l2);
  }
  ret = gwm_get_keyword (key, l, len, SCM_UNDEFINED);
  if (ret == SCM_UNDEFINED) {
    l2 = gwm_get_keyword (k_context, l, len, SCM_BOOL_F);
    if (gh_list_p(l2))
      return get_keyword(key, l2, default_value);
    else
      return default_value;
  } else
    return ret;
}

int wl_separate_context(SCM args, SCM& subret, SCM& ctxret, int& cn, const char *subr)
{
  int i, j, len;
  SCM ctx, sub, kctx;
  for (i=0, ctx=args, sub=SCM_EOL;
       SCM_NNULLP(ctx) && !(SCM_NIMP(SCM_CAR(ctx)) && scm_is_keyword(SCM_CAR(ctx)));
       ctx=SCM_CDR(ctx), i++)
    sub=scm_cons(SCM_CAR(ctx), sub);
  subret = scm_reverse_x(sub, SCM_EOL);
  kctx = ctx;
  cn = 0;
  while (gh_pair_p(kctx)) {
    cn += len = scm_ilength(kctx);
    if (len < 0 || (len & 1)) 
      gwm_misc_error(subr, "Bad keyword-value list: ~S", kctx);
    if ((kctx = gwm_get_keyword(k_context, kctx, len, SCM_BOOL_F)) != SCM_BOOL_F &&
        gh_list_p(kctx))
      ctx = gh_append2(ctx, kctx);
  }
  ctxret = ctx;
  for (j = 0; j < cn; j += 2) {
    if (!(SCM_NIMP(SCM_CAR(ctx)) && scm_is_keyword(SCM_CAR(ctx))))
      gwm_misc_error(subr, "Bad keyword: ~S", SCM_CAR(ctx));
    ctx = SCM_CDDR(ctx);
  }
  return i;
}

/* the host name as a string
 */

SCM_DEFINE(get_hostname, "hostname", 0, 0, 0,
           (),
           "Get the hostname.")
{
  return  GWM_host_name;
}

/* refreshes the screen or window if specified
 * very simple: creates/destroy a window just over the managed one
 */

SCM_DEFINE(gwm_refresh, "refresh", 1, 0, 0,
           (SCM arg),
           "Refresh the the given window or all windows on the screen.")
{
    XSetWindowAttributes wa;
    Window wd;
    unsigned long mask;
    Decoration* deco;

    if (!WLSCRWINP(arg))
      gwm_wrong_type_arg(s_gwm_refresh, 1, arg, "window or screen");
    deco = WL_DECO(arg);
    if (deco->Width() && deco->Height()) {
      wa.background_pixmap = None;
      wa.override_redirect = True;
      wa.backing_store = NotUseful;
      wa.save_under = False;
      mask = (CWBackPixmap | CWOverrideRedirect | CWBackingStore | CWSaveUnder);
      wd = XCreateWindow(dpy, deco->Xwin(), 0, 0, deco->Width(), deco->Height(),
                         0, CopyFromParent, CopyFromParent, CopyFromParent,
                         mask, &wa);
      XMapWindow(dpy, wd);
      XDestroyWindow(dpy, wd);
      XFlush(dpy);
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(set_screen_saver, "set-screen-saver!", 4, 0, 0,
           (SCM timeout, SCM interval, SCM preferblanking, SCM allowexposures),
           "Set screen saver parameters.")
{
    must_be_number(s_set_screen_saver, timeout, 1);
    must_be_number(s_set_screen_saver, interval, 2);
    XSetScreenSaver(dpy, gh_scm2int(timeout), gh_scm2int(interval),
		    (preferblanking == SCM_BOOL_F ? 0 : 1),
                    (allowexposures == SCM_BOOL_F ? 0 : 1));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(set_acceleration, "set-pointer-acceleration!", 2, 0, 0,
           (SCM num, SCM den),
           "Set pointer acceleration parameters.")
{
    must_be_number(s_set_acceleration, num, 1);
    must_be_number(s_set_acceleration, den, 2);
    XChangePointerControl(dpy, 1, 0, gh_scm2int(num), gh_scm2int(den), 0);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(set_threshold, "set-pointer-threshold!", 1, 0, 0,
           (SCM threshold),
           "Set number of pixels to move before pointer acceleration is used.")
{
    must_be_number(s_set_threshold, threshold, 1);
    XChangePointerControl(dpy, 0, 1, 1, 1, gh_scm2int(threshold));
    return SCM_UNSPECIFIED;
}

/* Keyboard bell ringer: (bell [volume percentage])
 */

SCM_DEFINE(ring_keyboard_bell, "bell", 0, 1, 0,
           (SCM arg),
           "Ring the keyboard bell. An optional argument is the volume in percent.")
{
    int             percent = 0;

    if (arg != SCM_UNDEFINED) {
	must_be_number(s_ring_keyboard_bell, arg, 1);
	percent = gh_scm2int(arg);
    }
    XBell(dpy, percent);
    return SCM_UNSPECIFIED;
}

/*
 * change mapping by specifiyng
 * keycode keysym keysym-mod1-mask keysym-mod2-mask ...
 * WARNING:  XChangeKeyboardMapping MALLOCS! (do not re-change same keys!)
 */
SCM_DEFINE(change_keyboard_mapping, "set-key-mapping!", 1, 0, 1,
           (SCM keycode, SCM args),
           "Change keyboard mapping for keycode to the given keysyms.")
{
    KeySym* keysyms;
    int i, argc;
    char* str;

    must_be_number(s_change_keyboard_mapping, keycode, 1);
    argc = scm_ilength(args);
    keysyms = new KeySym[argc];
    for (i = 0; i < argc; i++) {
      if (SCM_CAR(args) == SCM_BOOL_F) {
        keysyms[i] = NoSymbol;
      } else if (gh_string_p(SCM_CAR(args))) {
        str = wl_getstring(SCM_CAR(args), s_change_keyboard_mapping);
        keysyms[i] = XStringToKeysym(str);
        delete [] str;
        if (keysyms[i] == NoSymbol) {
          delete keysyms;
          gwm_misc_error(s_change_keyboard_mapping, "unknown key name: \"~A\"\n", SCM_CAR(args));
        }          
      } else {
        delete keysyms;
        gwm_wrong_type_arg(s_change_keyboard_mapping,
                           i+2, SCM_CAR(args), "string");
      }
      args = SCM_CDR(args);
    }
    XChangeKeyboardMapping(dpy, gh_scm2int(keycode), argc, keysyms, 1);
    delete keysyms;
    return SCM_UNSPECIFIED;
}

KeySym keycode_and_modifier_to_keysym(int keycode, int modifier)
{
  KeySym keysym;
  XKeyEvent xkey;
  xkey.type = KeyPress;
  xkey.display = dpy;
  xkey.state = modifier;
  xkey.keycode = keycode;
  XLookupString(&xkey, 0, 0, &keysym, 0);
  return keysym;
}

/* Is there any better way to do this ? */
int keysym_to_keycode_modifier_mask(KeySym keysym, int keycode)
{
  static unsigned int mode_shift_mask = 0;
  int i, shift;
  if (keycode == 0) {       // reset mode_shift_mask
    mode_shift_mask = 0;
    return 0;
  }
  for (i=0; i<4; i++)
    if (keysym == XKeycodeToKeysym(dpy, keycode, i))
      break;
  if (i>=4)
    return 0;
  shift = (i&1 ? ShiftMask : 0);
  if (i&2) {
    if (mode_shift_mask == 0) {
      // find mode_shift_mask
      for (i=1, mode_shift_mask = 2; i<32; i++, mode_shift_mask <<= 1)
        if (keysym == keycode_and_modifier_to_keysym(keycode, mode_shift_mask | shift))
          break;
    }
    return shift | mode_shift_mask;
  } else
    return shift;
}

/* translation beetween keysyms and keycodes
 */
SCM_DEFINE(keysym_to_keycode, "keysym->keycode", 1, 0, 0,
           (SCM keysym),
           "Convert the keysym name to a keycode and modifier pair.")
{
  KeySym sym;
  int code, mod;
  char* str;
  str = wl_getstring(keysym, s_keysym_to_keycode, 1);
  sym = XStringToKeysym(str);
  delete [] str;
  if (sym == NoSymbol)
    return SCM_BOOL_F;
  code = XKeysymToKeycode(dpy, sym);
  mod = keysym_to_keycode_modifier_mask(sym, code);
  return scm_cons(gh_int2scm(code), gh_int2scm(mod));
}

SCM_DEFINE(keycode_to_keysym, "keycode->keysym", 2, 0, 0,
           (SCM keycode, SCM mod),
           "Convert a keycode and modifier to a keysym name.")
{
  char* name;
  must_be_number(s_keycode_to_keysym, keycode, 1);
  must_be_number(s_keycode_to_keysym, mod, 2);
  name = XKeysymToString(keycode_and_modifier_to_keysym(gh_scm2int(keycode), 
                                                        gh_scm2int(mod)));
  if (name)
    return scm_makfrom0str(name);
  else
    return SCM_BOOL_F;
}

/*
 * Selection interface:
 * Active values:
 * 	cut-buffer: access to cut-buffer 0 (for xterm use)
 */

SCM_DEFINE(get_cut_buffer_0, "cut-buffer", 0, 0, 0,
           (),
           "Get contents of X cut buffer number 0.")
{
  int nbytes;
  char* buffer;
  SCM string_of_buffer;
  buffer = XFetchBytes(dpy, &nbytes);
  if (buffer) {
    string_of_buffer = scm_makfrom0str(buffer);
    XFree(buffer);
    return string_of_buffer;
  } else {
    return scm_makfrom0str("");
  }
}

SCM_DEFINE(set_cut_buffer_0, "set-cut-buffer!", 1, 0, 0,
           (SCM string),
           "Set contents of X cut buffer number 0.")
{
  must_be_string(s_set_cut_buffer_0, string, 1);
  XStoreBytes(dpy, scm_i_string_chars(string), scm_i_string_length(string));
  XSetSelectionOwner(dpy, XA_PRIMARY, None, CurrentTime);
  return SCM_UNSPECIFIED;
}

/* buffer i becomes buffer i+incr %8 */

SCM_DEFINE(rotate_cut_buffers, "rotate-cut-buffers", 0, 1, 0,
           (SCM arg),
           "Rotate the cut buffers arg steps.")
{
    int             incr = 1;

    if (arg != SCM_UNDEFINED) {
	must_be_number(s_rotate_cut_buffers, arg, 1);
	incr = gh_scm2int(arg);
    }
    XRotateBuffers(dpy, incr);
    return SCM_UNSPECIFIED;
}

/*
 * get any property of current window
 */

SCM_DEFINE(get_window_property, "get-x-property", 2, 1, 0,
           (SCM window, SCM name, SCM del),
           "Get X property name from window. Delete if del is given and non-false.")
{
    Atom            actual_type;
    Atom            property_name;
    int             actual_format;
    unsigned long   nitems;
    unsigned long   bytes_after;
    unsigned char  *buffer = 0;
    int i;
    unsigned long len;
    Window hook;
    SCM     result = SCM_BOOL_F;
    int dl;
    char* str;
    if (!WLSCRWINP(window))
      gwm_wrong_type_arg(s_get_window_property, 1, window, "window or screen");
    str = wl_getsymbol(name, s_get_window_property, 2);
    if (WLWINDOWP(window))
      hook = (WL_WINDOW(window)->PendingClose() ? 0 : WL_WINDOW(window)->InnerWin());
    else
      hook = WL_SCREEN(window)->Root();
    dl = (del != SCM_UNDEFINED && del != SCM_BOOL_F);
    property_name = XInternAtom(dpy, str, False);
    delete [] str;
    if (hook &&
	Success == XGetWindowProperty(dpy, hook,
                                      property_name, 0, 1024,
				      dl, AnyPropertyType, &actual_type,
                                      &actual_format, &nitems, &bytes_after,
                                      &buffer)
	&& buffer && nitems) {
      if (bytes_after)
        XGetWindowProperty(dpy, hook,
                           property_name, 0, 1024 + bytes_after/4 + 1,
                           dl, AnyPropertyType, &actual_type,
                           &actual_format, &nitems, &bytes_after,
                           &buffer);
      switch (actual_type) {
      case XA_STRING:
        len = strlen((char*) buffer) + 1;
        result = scm_makfrom0str((char*) buffer);
        if (len < nitems) {
          result = scm_cons(result, SCM_EOL);
          while (len < nitems) {
            result = scm_cons(scm_makfrom0str(((char*) buffer) + len), result);
            len += strlen((char*)buffer+len) + 1;
          }
          result = scm_reverse_x(result, SCM_EOL);
        }
        break;
      case XA_INTEGER:
      case XA_CARDINAL:
        if (nitems == 1)
          result = gh_int2scm(actual_format == 16 ?
                              *((short int *) buffer) :
                              *((long int *) buffer));
        else {
          result = SCM_EOL;
          if (actual_format == 16)
            for (i=nitems-1; i>=0; i--)
              result = scm_cons(gh_int2scm(((short int *) buffer)[i]), result);
          else
            for (i=nitems-1; i>=0; i--)
              result = scm_cons(gh_int2scm(((long int *) buffer)[i]), result);
        }
        break;
      case XA_ATOM:
        if (nitems == 1)
          result = gh_symbol2scm(XGetAtomName(dpy, *((Atom*) buffer)));
        else {
          result = SCM_EOL;
          for (i=nitems-1; i>=0; i--) {
            result = scm_cons(gh_symbol2scm(XGetAtomName(dpy, *(((Atom*) buffer)+i))), result);
          }
        }
        break;
      }
      XFree(buffer);
    }
    return result;
}

SCM_DEFINE(set_window_property, "set-x-property!", 3, 1, 0,
           (SCM window, SCM name, SCM value, SCM app),
           "Set X property name on window to value. Append if app is given and non-false.")
{
    Atom            actual_type;
    Atom            property_name;
    Atom            aval;
    int             actual_format;
    unsigned long   nitems;
    unsigned char  *buffer;
    Window          hook;
    long int        num;
    int malloced=0;
    unsigned long i;
    SCM lst;
    char* p;
    char* str;
    if (!WLSCRWINP(window))
      gwm_wrong_type_arg(s_set_window_property, 1, window, "window or screen");
    str = wl_getsymbol(name, s_set_window_property, 2);
    if (WLWINDOWP(window))
      hook = (WL_WINDOW(window)->PendingClose() ? 0 : WL_WINDOW(window)->InnerWin());
    else
      hook = WL_SCREEN(window)->Root();
    property_name = XInternAtom(dpy, str, False);
    delete [] str;
    if (gh_string_p(value)) {
      actual_type = XA_STRING;
      actual_format = 8;
      buffer = (unsigned char *) wl_getstring(value, s_set_window_property);
      nitems = strlen((char*)buffer) + 1;
      malloced = 1;
    } else if (gh_symbol_p(value)) {
      actual_type = XA_ATOM;
      actual_format = 32;
      str = wl_getsymbol(value, s_set_window_property);
      aval = XInternAtom(dpy, str, False);
      delete [] str;
      buffer = (unsigned char *) &aval;
      nitems = 1;
    } else if (scm_is_integer(value)) {
      actual_type = XA_CARDINAL;
      actual_format = 32;
      num = gh_scm2int(value);
      buffer = (unsigned char *) &num;
      nitems = 1;
    } else if (value == SCM_EOL) {
      actual_type = XA_CARDINAL;
      actual_format = 32;
      nitems = 0;
      buffer = (unsigned char *) &num;
    } else if (gh_list_p(value)) {
      if (gh_string_p(SCM_CAR(value))) {
	actual_type = XA_STRING;
	actual_format = 8;
        nitems = 0;
        for (i=scm_ilength(value), lst=value; i>0; i--, lst=SCM_CDR(lst))
          if (gh_string_p(SCM_CAR(lst)))
            nitems += scm_i_string_length(SCM_CAR(lst)) + 1;
          else
            gwm_wrong_type_arg(s_set_window_property , 0, SCM_CAR(lst), "string");
        buffer = new unsigned char[nitems];
        malloced = 1;
        p = (char*) buffer;
        for (i=scm_ilength(value), lst=value; i>0; i--, lst=SCM_CDR(lst)) {
          strncpy(p, scm_i_string_chars(SCM_CAR(lst)), scm_i_string_length(SCM_CAR(lst)));
          p += scm_i_string_length(SCM_CAR(lst)) + 1;
          p[-1] = 0;
        }
      } else if (gh_symbol_p(SCM_CAR(value))) {
	actual_type = XA_ATOM;
	actual_format = 32;
	nitems = scm_ilength(value);
        buffer = new unsigned char[sizeof(long int)*nitems];
        malloced = 1;
        for (i=0, lst=value; i<nitems; i++, lst=SCM_CDR(lst))
          if (gh_symbol_p(SCM_CAR(lst))) {
            str = wl_getsymbol(SCM_CAR(lst), s_set_window_property);
            ((Atom*) buffer)[i] = XInternAtom(dpy, str, False);
            delete [] str;
          } else {
            delete [] buffer;
            gwm_wrong_type_arg(s_set_window_property , 0, SCM_CAR(lst), "symbol");
          }
      } else if (scm_is_integer(SCM_CAR(value))) {
	actual_type = XA_CARDINAL;
	actual_format = 32;
	nitems = scm_ilength(value);
        buffer = new unsigned char[sizeof(long int)*nitems];
        malloced = 1;
        for (i=0, lst=value; i<nitems; i++, lst=SCM_CDR(lst))
          if (scm_is_integer(SCM_CAR(lst)))
            ((long int*) buffer)[i] = gh_scm2int(SCM_CAR(lst));
          else {
            delete [] buffer;
            gwm_wrong_type_arg(s_set_window_property , 0, SCM_CAR(lst), "number");
          }
      } else
        gwm_wrong_type_arg(s_set_window_property , 3, value, "list of strings, symbols or numbers");
    } else {
	gwm_wrong_type_arg(s_set_window_property , 3, value, "string, symbol, number or list");
    }

    if (hook)
      XChangeProperty(dpy, hook,
                      property_name, actual_type, actual_format,
                      (app != SCM_UNDEFINED && app != SCM_BOOL_F ?
                       PropModeAppend : PropModeReplace),
                      buffer, nitems);
    if (malloced)
      delete [] buffer;
    return SCM_UNSPECIFIED;
}

/*
 * interface to XGetDefault
 */

SCM_DEFINE(get_x_default, "get-x-default", 2, 0, 0,
           (SCM program, SCM option),
           "Get values from the X resource database.")
{
    char* result;
    char *str1, *str2;
    str1 = wl_getstring(program, s_get_x_default, 1);
    str2 = wl_getstring(option, s_get_x_default, 2);
    result = XGetDefault(dpy, str1, str2);
    delete [] str1;
    delete [] str2;
    if (result)
	return scm_makfrom0str(result);
    else
	return SCM_BOOL_F;
}

/* find out if window already has existed on screen */

SCM_DEFINE(window_was_on_screen, "window-was-on-screen", 0, 1, 0,
           (SCM window),
           "Returns true if the window was on screen before Gwm started.")
{
  ClientWindow* cw;
  must_be_window(s_window_was_on_screen, window, 1);
  cw = WL_WINDOW(window);
  if (GWM_new_window_id != cw->InnerWin())
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(gwm_is_starting, "gwm-is-starting", 0, 0, 0,
           (),
           "Returns true if Gwm has not yet finished opening all screens.")
{
  if (GWM_is_starting)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(gwm_is_ending, "gwm-is-ending", 0, 0, 0,
           (),
           "Returns true if Gwm is currently closing all screens.")
{
  if (GWM_is_ending)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/* returns the list of managed screens
 */

SCM_DEFINE(get_list_of_screens, "list-of-screens", 0, 0, 0,
           (),
           "Get the list of all managed screens.")
{
  ScreenContext* scr;
  SCM wl_list = SCM_EOL;
  FOR_ALL_SCREENS(scr) 
    wl_list = scm_cons(scr->Deco()->scm(), wl_list);
  return scm_reverse_x(wl_list, SCM_EOL);
}

/* returns the list of clientwindows or realized icons on screen,
 * mapped or not
 */
SCM_DEFINE(get_list_of_windows, "list-of-windows", 0, 0, 1,
           (SCM args),
           "Get a list of managed windows. Possible arguments are:"
           "'window          only windows are returned (not icons)"
           "'icon            only icons"
           "'mapped          only mapped windows"
           "'stacking-order  sorted in stacking order"
           "'all-screens     windows on all screens (default is only on current)"
           "screen           only on this screen")
{
  unsigned int i, argc;
  int j, scr_a, scr_b;
  SCM wl_list;
  Decoration* wob;
  ClientWindow* cw;
  ScreenContext* scr;
  int status = 0;		/* 'window or 'icon */
  int mapped = 0;		/* 'mapped for only visible ones */
  int stacking_order = 0;	/* do a XQueryTree to get the list */
  int all_screens = 0;        /* windows from all screens */
  unsigned int nb_windows;
  Window *windows, dummywin, parent;

  argc = scm_ilength(args);
  scr_b = Context->ScreenNum();
  scr_a = scr_b - 1;
  for (i = 0; i < argc; i++) {	/* options parsing */
    if (SCM_CAR(args) == WA_window)
      status = WindowStatus;
    else if (SCM_CAR(args) == WA_icon)
      status = IconStatus;
    else if (SCM_CAR(args) == WA_mapped)
      mapped = 1;
    else if (SCM_CAR(args) == WA_stacking_order)
      stacking_order = 1;
    else if (SCM_CAR(args) == WA_all_screens) {
      all_screens = 1;
      scr_b = GWM_ScreenCount - 1;
      scr_a = -1;
    } else if (WLSCREENP(SCM_CAR(args)) && !all_screens) {
      all_screens = -1;
      scr_b = WL_SCREEN(SCM_CAR(args))->ScreenNum();
      scr_a = scr_b - 1;
    } else {
      if (!gh_symbol_p(SCM_CAR(args)))
        gwm_wrong_type_arg(s_get_list_of_windows, i+1, SCM_CAR(args), "symbol");
      else
        gwm_misc_error(s_get_list_of_windows, "unknown symbol ~S", SCM_CAR(args));
    }
    args = SCM_CDR(args);
  }

  wl_list = SCM_EOL;
  for (j=scr_b; j>scr_a; j--) {
    scr = GWMManagedScreens[j];
    if (scr == NULL) continue;
    if (stacking_order) {
      XQueryTree(dpy, scr->Root(), &dummywin, &parent,
      	   &windows, &nb_windows);
      if (nb_windows) {
        for (i=0; i<nb_windows; i++)
          if ((wob = LookUpDeco(windows[i]))
              && !wob->Parent() && wob->Win())
            if ((status == 0 || (status & wob->Type())) &&
                (!mapped || (wob->Type() & IconStatus ? 
                             wob->Win()->MappedIcon() :
                             wob->Win()->MappedWin())) &&
                !wob->Win()->IsClosing())
              wl_list = scm_cons(wob->scm(), wl_list);
        XFree(windows);
      }
    } else {
      cw = scr->First();
      while (cw) {
        if ((status == 0 || (status & WindowStatus)) &&
            (!mapped || cw->MappedWin()))
          wl_list = scm_cons(cw->Deco()->scm(), wl_list);
        if ((status == 0 || (status & IconStatus)) &&
            (!mapped || cw->MappedIcon()) &&
            cw->Icon())
          wl_list = scm_cons(cw->Icon()->scm(), wl_list);
        cw = cw->Next();
      }
    }
  }
  return scm_reverse_x(wl_list, SCM_EOL);
}

SCM_DEFINE(get_deco_at_position, "deco-at-position", 2, 1, 0,
           (SCM x, SCM y, SCM scr),
           "Return the decoration at position (x, y) on the screen.")
{
  int xp, yp;
  int xr, yr;
  Window window1, window2;
  ScreenContext* screen;
  Decoration *wob1, *wob2;
  IClient* cl;

  must_be_number(s_get_deco_at_position, x, 1);
  must_be_number(s_get_deco_at_position, y, 2);
  if (scr == SCM_UNDEFINED)
    screen = Context;
  else if (WLSCREENP(scr))
    screen = WL_SCREEN(scr);
  else
    gwm_wrong_type_arg(s_get_deco_at_position, 3, scr, "screen");
  xp = gh_scm2int(x);
  yp = gh_scm2int(y);
  window1 = screen->Root();
  wob1 = screen->Deco();
  while (XTranslateCoordinates(dpy, screen->Root(), window1,
                               xp, yp, &xr, &yr, &window2)) {
    if (window2 == None || !(wob2 = LookUpDeco(window2)))
      break;
    window1 = window2;
    wob1 = wob2;
  }
  if ((cl = LookUpInnerClient(window2)))
    return cl->scm();
  else
    return wob1->scm();
}

SCM_DEFINE(get_window_at_position, "window-at-position", 2, 1, 0,
           (SCM x, SCM y, SCM scr),
           "Return the window at position (x, y) on the screen.")
{
  int xr, yr;
  Window window;
  ScreenContext* screen;
  Decoration* wob;
  IClient* cl;

  must_be_number(s_get_window_at_position, x, 1);
  must_be_number(s_get_window_at_position, y, 2);
  if (scr == SCM_UNDEFINED)
    screen = Context;
  else if (WLSCREENP(scr))
    screen = WL_SCREEN(scr);
  else
    gwm_wrong_type_arg(s_get_window_at_position, 3, scr, "screen");
  if (XTranslateCoordinates(dpy, screen->Root(), screen->Root(),
      		      gh_scm2int(x), gh_scm2int(y), &xr, &yr, &window)) {
    if ((wob = LookUpDeco(window)))
      return wob->Top()->scm();
    else if ((cl = LookUpInnerClient(window)))
      return cl->Top()->scm();
  }
  return SCM_BOOL_F;
}

/*
 * returns [x y modifiers screen]
 */

SCM_DEFINE(current_mouse_position, "pointer-position", 0, 0, 0,
           (),
           "Return a list with current x, y, modifiers, and screen of the pointer.")
{
  unsigned int ptrmask;	/* state of ptr when queried */
  Window root, sub_window;
  int root_x, root_y, cur_x, cur_y;
  int rootind;
  SCM wl_list, scr;

  root=0;
  XQueryPointer(dpy, Context->Root(), &root, &sub_window,
                &root_x, &root_y, &cur_x, &cur_y, &ptrmask);
  if (root &&
      (rootind = ScreenOfRoot(root)) >= 0 &&
      GWMManagedScreens[rootind])
    scr = GWMManagedScreens[rootind]->Deco()->scm();
  else
    scr = SCM_BOOL_F;
  wl_list = gh_list(gh_int2scm(root_x), gh_int2scm(root_y),
                    gh_int2scm(ptrmask),
                    scr,
                    SCM_UNDEFINED);
  return wl_list;
}

/* warps pointer to pos
 * (warp-pointer x y [window-relative-to])
 */

SCM_DEFINE(warp_pointer, "warp-pointer", 2, 1, 0,
           (SCM x, SCM y, SCM arg),
           "Move pointer to (x, y) relative to the current position, or relative"
           "to the decoration in the optional argument.")
{
  Window          dest_w = None;

  must_be_number(s_warp_pointer, x, 1);
  must_be_number(s_warp_pointer, y, 2);
  if (arg != SCM_UNDEFINED) {
      must_be_valid_deco(s_warp_pointer, arg, 3);
      dest_w = (WL_DECO(arg))->Xwin();
  }
  XWarpPointer(dpy, None, dest_w,
               0, 0, 0, 0, gh_scm2int(x), gh_scm2int(y));
  return SCM_UNSPECIFIED;
}


/*
 * width & height of any object
 */

void wl_get_dimensions(SCM obj, Box* box)
{
  if (WLACTIVEP(obj))
    WL_ACTIVE(obj)->dimensions(box);
  else if (WLPIXMAPP(obj))
    WL_PIXMAP(obj)->dimensions(box);
  else if (WLDECOP(obj))
    WL_DECO(obj)->dimensions(box);
  else
    scm_wrong_type_arg(0, 1, obj);
}

SCM_DEFINE(gwm_width, "width", 1, 0, 0,
           (SCM object),
           "Get the width of obj (a pixmap, active pixmap, or deco)")
{
  Box box;
  wl_get_dimensions(object, &box);
  return gh_int2scm(box.width);
}

SCM_DEFINE(gwm_height, "height", 1, 0, 0,
           (SCM object),
           "Get the height of obj (a pixmap, active pixmap, or deco)")
{
  Box box;
  wl_get_dimensions(object, &box);
  return gh_int2scm(box.height);
}

SCM_DEFINE(gwm_dimensions, "dimensions", 1, 0, 0,
           (SCM object),
           "Get the dimensions of obj (a pixmap, active pixmap, or deco).")
{
  Box box;
  SCM result;
  wl_get_dimensions(object, &box);
  result = gh_list(gh_int2scm(box.x), gh_int2scm(box.y),
                   gh_int2scm(box.width), gh_int2scm(box.height),
                   SCM_UNDEFINED);
  return result;
}

SCM_DEFINE(register_gnome_compliance, "register-gnome-compliance", 0, 0, 0,
           (),
           "Register gnome compliance, i.e. signal to the clients that some"
           "gnome compliance is supported.")
{
  Window win = Context->GwmWindow();
  if (!GWM_WidgetMode) {
    XChangeProperty(dpy, Context->Root(), XA_GNOME_SUPPORTING, XA_CARDINAL,
                    32, PropModeReplace, (unsigned char *) &win, 1);
    XChangeProperty(dpy, win, XA_GNOME_SUPPORTING, XA_CARDINAL,
                    32, PropModeReplace, (unsigned char *) &win, 1);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(unregister_gnome_compliance, "unregister-gnome-compliance", 0, 0, 0,
           (),
           "Unregister gnome compliance.")
{
  if (!GWM_WidgetMode) {
    XDeleteProperty(dpy, Context->Root(), XA_GNOME_SUPPORTING);
    XDeleteProperty(dpy, Context->GwmWindow(), XA_GNOME_SUPPORTING);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(check_gnome_compliance, "check-gnome-compliance", 0, 0, 0,
           (),
           "Check if gnome compliance is currently registered.")
{
  Atom actual_type;
  int actual_format;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *buffer = 0;
  if (!GWM_WidgetMode &&
      Success == XGetWindowProperty(dpy, Context->GwmWindow(),
                                    XA_GNOME_SUPPORTING, 0, 4,
                                    False, XA_CARDINAL, &actual_type,
                                    &actual_format, &nitems, &bytes_after,
                                    &buffer) &&
      nitems && buffer) {
    XFree(buffer);
    return SCM_BOOL_T;
  } else
    return SCM_BOOL_F;
}

SCM_DEFINE(register_gnome_feature, "register-gnome-feature", 1, 0, 0,
           (SCM sym),
           "Signal to the clients that the feature sym (a symbol) is supported by"
           "this Gwm profile.")
{
  Atom actual_type;
  Atom property_name;
  int actual_format;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *buffer = 0;
  unsigned int i;
  char* str;
  if (GWM_WidgetMode)
    return SCM_UNSPECIFIED;
  str = wl_getsymbol(sym, s_register_gnome_feature, 1);
  property_name = XInternAtom(dpy, str, False);
  delete [] str;
  if (Success == XGetWindowProperty(dpy, Context->Root(),
                                    XA_GNOME_PROTOCOLS, 0, 1024,
                                    False, XA_ATOM, &actual_type,
                                    &actual_format, &nitems, &bytes_after,
                                    &buffer) &&
      nitems && buffer) {
    for (i=0; i<nitems; i++)
      if (((Atom*) buffer)[i] == property_name)
        break;
    if (i == nitems) {
      XChangeProperty(dpy, Context->Root(), XA_GNOME_PROTOCOLS, XA_ATOM,
                      32, PropModeAppend, (unsigned char *) &property_name, 1);
    }
    XFree(buffer);
  } else {
    XChangeProperty(dpy, Context->Root(), XA_GNOME_PROTOCOLS, XA_ATOM,
                    32, PropModeReplace, (unsigned char *) &property_name, 1);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(unregister_gnome_feature, "unregister-gnome-feature", 1, 0, 0,
           (SCM sym),
           "Signal to the clients that the feature sym (a symbol) is not supported"
           "by this Gwm profile.")
{
  Atom actual_type;
  Atom property_name;
  int actual_format;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *buffer = 0;
  unsigned int i;
  char* str;
  if (GWM_WidgetMode)
    return SCM_UNSPECIFIED;
  str = wl_getsymbol(sym, s_unregister_gnome_feature, 1);
  property_name = XInternAtom(dpy, str, False);
  delete [] str;
  if (Success == XGetWindowProperty(dpy, Context->Root(),
                                    XA_GNOME_PROTOCOLS, 0, 1024,
                                    False, XA_ATOM, &actual_type,
                                    &actual_format, &nitems, &bytes_after,
                                    &buffer) &&
      nitems && buffer) {
    for (i=0; i<nitems; i++)
      if (((Atom*) buffer)[i] == property_name)
        break;
    if (i<nitems) {
      for (; i<nitems-1; i++)
        ((Atom*) buffer)[i] = ((Atom*) buffer)[i+1];
      if (nitems > 1)
        XChangeProperty(dpy, Context->Root(), XA_GNOME_PROTOCOLS, XA_ATOM,
                        32, PropModeReplace, buffer, nitems-1);
      else
        XDeleteProperty(dpy, Context->Root(), XA_GNOME_PROTOCOLS);
    }
    XFree(buffer);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(check_gnome_feature, "check-gnome-feature", 1, 0, 0,
           (SCM sym),
           "Check if the gnome feature sym is supported by this Gwm profile.")
{
  Atom actual_type;
  Atom property_name;
  int actual_format;
  unsigned long nitems;
  unsigned long bytes_after;
  unsigned char *buffer = 0;
  unsigned int i;
  char* str;
  if (GWM_WidgetMode)
    return SCM_BOOL_F;
  str = wl_getsymbol(sym, s_check_gnome_feature, 1);
  property_name = XInternAtom(dpy, str, False);
  delete [] str;
  if (Success == XGetWindowProperty(dpy, Context->Root(),
                                    XA_GNOME_PROTOCOLS, 0, 1024,
                                    False, XA_ATOM, &actual_type,
                                    &actual_format, &nitems, &bytes_after,
                                    &buffer) &&
      nitems && buffer) {
    for (i=0; i<nitems; i++)
      if (((Atom*) buffer)[i] == property_name) {
        XFree(buffer);
        return SCM_BOOL_T;
      }
    XFree(buffer);
    return SCM_BOOL_F;
  } else
    return SCM_BOOL_F;
}

SCM_DEFINE(GWM_execute, "execute", 0, 0, 1,
           (SCM args),
           "Execute the command given by the arguments. Returns process id. Use"
           "the function 'waitpid' to wait for synchronous execution.")
{
  int argc, i;
  char **argv = 0;
  int pid;
  argc = scm_ilength(args);
  if (argc) {				/* pass args */
    argv = new char*[argc + 1];
    for (i = 0; i < argc; i++) {
      if (!gh_string_p(SCM_CAR(args))) {
        for (int j=0; j<i; j++)
          delete [] argv[j];
        delete [] argv;
        gwm_wrong_type_arg(s_GWM_execute, i+1, SCM_CAR(args), "string");
      }
      argv[i] = wl_getstring(SCM_CAR(args), s_GWM_execute);
      args = SCM_CDR(args);
    }
    argv[argc] = 0;
  } else {
    gwm_wrong_num_args(s_GWM_execute, 0);
  }
  if (!(pid = fork())) {
    close(XConnectionNumber(dpy));
    scm_close_all_ports_except(scm_listify(scm_current_output_port(), scm_current_error_port(), SCM_UNDEFINED));
    setpgid(0, 0);
    if (GWM_ScreenCount > 1) {
      char* display = getenv("DISPLAY");
      char* pos = strchr(display, ':');
      pos = (pos ? strchr(pos, '.') : 0);
      if (pos)
        *(pos+1) = Context->ScreenNum() + '0'; // Just put it in, no range checks...
    }
    execvp(argv[0], argv);
    exit(127);
  }
  for (i=0; i<argc; i++)
    delete [] argv[i];
  delete [] argv;
  return gh_int2scm(pid);
}

/*
 * restarts GWM the hard way
 */

SCM_DEFINE(GWM_restart, "restart", 0, 0, 1,
           (SCM args),
           "Restart Gwm, or start the program given by the optional arguments.")
{
  int argc, i;
  ScreenContext* scr;
  argc = scm_ilength(args);
  if (argc) {				/* pass args */
    char **new_argv = new char*[argc + 1];
    int i;
    for (i = 0; i < argc; i++) {
      if (!gh_string_p(SCM_CAR(args))) {
        for (int j=0; j<i; j++)
          delete [] new_argv[j];
        delete [] new_argv;
        gwm_wrong_type_arg(s_GWM_restart, i+1, SCM_CAR(args), "string");
      }
      new_argv[i] = wl_getstring(SCM_CAR(args), s_GWM_restart);
      args = SCM_CDR(args);
    }
    new_argv[argc] = 0;
    GWM_restart_argv = new_argv;
  } else {				/* same args */
    GWM_restart_argv = GWM_argv;
  }
  if (!GWM_WidgetMode)
    GWM_re_init_PointerRoot();
  XSync(dpy, True);
  if (!GWM_is_ending) {
    GWM_is_ending = 1;
    if (GWM_rubber_feedback)
      UnDrawRubber();
    if (Fsm::ServerGrabbed())
      Fsm::ClearGrabs();
    FOR_ALL_SCREENS(scr)
      scr->Close();
  }
  XCloseDisplay(dpy);
#ifdef HAVE_READLINE
  rl_free_line_state ();
  rl_cleanup_after_signal();
  rl_deprep_terminal ();
#endif
  execvp(GWM_restart_argv[0], GWM_restart_argv);
  gwm_misc_error(s_GWM_restart, "could not restart ~A", scm_makfrom0str(GWM_restart_argv[0]));
  return SCM_UNSPECIFIED; /* Not reached */
}

SCM_REGISTER_PROC(s_user_quit, "quit", 0, 0, 0, user_end);
SCM_DEFINE(user_end, "end", 0, 0, 0,
           (),
           "Quit Gwm.")
{
  if(!GWM_Quiet)
    gwm_puts("Bye.\n");
  gwm_end(0);
  return SCM_UNSPECIFIED; /* Not reached */
}

/* this function allows a kind of multitasking:
 * It recusrivly processes pending events in the queue and returns as soon as
 * the queue is empty
 * Can be used during long tedious tasks so as not to "freeze" gwm
 */

SCM_DEFINE(process_events, "process-events", 0, 0, 0,
           (),
           "Recursively process events as long as there are any in the queue.")
{
  XSync(dpy, 0);
  GWM_ProcessEvents(0);
  return SCM_UNSPECIFIED;
}
/*
 * functions to process events in the queue
 */

void process_masked_events(int mask)
{
  XEvent evt;
  Decoration* wob;
  ClientWindow* cw;
  
  while (XCheckMaskEvent(dpy, mask, &evt)) {
    if ((wob = LookUpDeco(evt.xany.window)))
      wob->EventHandler(&evt);
    else if ((cw = LookUpClient(evt.xany.window)))
      cw->EventHandler(&evt);
  }
}

SCM_DEFINE(process_exposes, "process-exposes", 0, 0, 0,
           (),
           "Process any expose events.")
{
  XSync(dpy, 0);
  process_masked_events(ExposureMask);
  return SCM_UNSPECIFIED;
}


    SCM_GLOBAL_VARIABLE_INIT(v_gwm_quiet, "gwm-quiet", (GWM_Quiet ? SCM_BOOL_T : SCM_BOOL_F));
    SCM_GLOBAL_VARIABLE_INIT(v_double_click_delay, "double-click-delay", gh_int2scm(300));
    SCM_GLOBAL_VARIABLE_INIT(v_relevant_modifiers, "relevant-modifiers", gh_int2scm(255));

    SCM_GLOBAL_SYMBOL(WA_icon, "icon");
    SCM_GLOBAL_SYMBOL(WA_window, "window"); /* proc also */
    SCM_GLOBAL_SYMBOL(WA_mapped, "mapped");
    SCM_GLOBAL_SYMBOL(WA_stacking_order, "stacking-order");
    SCM_GLOBAL_SYMBOL(WA_all_screens, "all-screens");
    SCM_GLOBAL_SYMBOL(WA_mono, "mono");
    SCM_GLOBAL_SYMBOL(WA_gray, "gray");
    SCM_GLOBAL_SYMBOL(WA_color, "color");
    SCM_GLOBAL_SYMBOL(WA_transparent, "transparent");
    SCM_GLOBAL_SYMBOL(WA_hole, "hole");
    SCM_GLOBAL_SYMBOL(WA_x, "x");
    SCM_GLOBAL_SYMBOL(WA_y, "y");
    SCM_GLOBAL_SYMBOL(WA_static, "static");
    SCM_GLOBAL_SYMBOL(WA_forget, "forget");
    SCM_GLOBAL_SYMBOL(WA_northwest, "northwest");
    SCM_GLOBAL_SYMBOL(WA_north, "north");
    SCM_GLOBAL_SYMBOL(WA_northeast, "northeast");
    SCM_GLOBAL_SYMBOL(WA_west, "west");
    SCM_GLOBAL_SYMBOL(WA_center, "center");
    SCM_GLOBAL_SYMBOL(WA_east, "east");
    SCM_GLOBAL_SYMBOL(WA_southwest, "southwest");
    SCM_GLOBAL_SYMBOL(WA_south, "south");
    SCM_GLOBAL_SYMBOL(WA_southeast, "southeast");
    SCM_GLOBAL_SYMBOL(WA_horizontal, "horizontal");
    SCM_GLOBAL_SYMBOL(WA_vertical, "vertical");
    SCM_GLOBAL_SYMBOL(WA_on_event, "on-event");
    SCM_GLOBAL_SYMBOL(WA_event, "event");
    SCM_GLOBAL_SYMBOL(WA_deco, "deco");

    SCM_GLOBAL_KEYWORD(k_direction, "direction");
    SCM_GLOBAL_KEYWORD(k_min_width, "min-width");
    SCM_GLOBAL_KEYWORD(k_max_width, "max-width");
    SCM_GLOBAL_KEYWORD(k_min_height, "min-height");
    SCM_GLOBAL_KEYWORD(k_max_height, "max-height");
    SCM_GLOBAL_KEYWORD(k_separator, "separator");
    SCM_GLOBAL_KEYWORD(k_margin, "margin");
    SCM_GLOBAL_KEYWORD(k_borderwidth, "borderwidth");
    SCM_GLOBAL_KEYWORD(k_bordercolor, "bordercolor");
    SCM_GLOBAL_KEYWORD(k_background, "background");
    SCM_GLOBAL_KEYWORD(k_foreground, "foreground");
    SCM_GLOBAL_KEYWORD(k_behavior, "behavior");
    SCM_GLOBAL_KEYWORD(k_anchor, "anchor");
    SCM_GLOBAL_KEYWORD(k_gravity, "gravity");
    SCM_GLOBAL_KEYWORD(k_cursor, "cursor");
    SCM_GLOBAL_KEYWORD(k_property, "property");
    SCM_GLOBAL_KEYWORD(k_font, "font");
    SCM_GLOBAL_KEYWORD(k_horizontal_margin, "horizontal-margin");
    SCM_GLOBAL_KEYWORD(k_vertical_margin, "vertical-margin");
    SCM_GLOBAL_KEYWORD(k_xpm_closeness, "xpm-closeness");
    SCM_GLOBAL_KEYWORD(k_shape, "shape");
    SCM_GLOBAL_KEYWORD(k_angle, "angle");
    SCM_GLOBAL_KEYWORD(k_mirrored, "mirrored");
    SCM_GLOBAL_KEYWORD(k_rotate, "rotate");
    SCM_GLOBAL_KEYWORD(k_crop, "crop");
    SCM_GLOBAL_KEYWORD(k_color, "color");
    SCM_GLOBAL_KEYWORD(k_width, "width");
    SCM_GLOBAL_KEYWORD(k_height, "height");
    SCM_GLOBAL_KEYWORD(k_context, "context");
    SCM_GLOBAL_KEYWORD(k_name, "name");   
    SCM_GLOBAL_KEYWORD(k_icon_name, "icon-name");    
    SCM_GLOBAL_KEYWORD(k_class_name, "class-name");   
    SCM_GLOBAL_KEYWORD(k_client_name, "client-name");  
    SCM_GLOBAL_KEYWORD(k_decoration, "decoration");
    SCM_GLOBAL_KEYWORD(k_icon_decoration, "icon-decoration");
    SCM_GLOBAL_KEYWORD(k_no_freeze, "no-freeze");
    SCM_GLOBAL_KEYWORD(k_grab_keyboard, "grab-keyboard");
    SCM_GLOBAL_KEYWORD(k_confine_pointer, "confine-pointer");
    SCM_GLOBAL_KEYWORD(k_grab_children, "grab-children");
    SCM_GLOBAL_KEYWORD(k_menu_parent, "menu-parent");
    SCM_GLOBAL_KEYWORD(k_steal, "steal");
    SCM_GLOBAL_KEYWORD(k_resend, "resend");
    SCM_GLOBAL_KEYWORD(k_propagate, "propagate");
    SCM_GLOBAL_KEYWORD(k_cached, "cached");
    SCM_GLOBAL_KEYWORD(k_invert_color, "invert-color");

SCM v_describe_window;
SCM v_describe_icon;
SCM v_describe_screen;
SCM v_move_window;
SCM v_resize_window;
SCM v_move_resize_window;
SCM v_raise_window;
SCM v_lower_window;
SCM v_iconify_window;
SCM v_de_iconify_window;
SCM v_load_path;

/* Here goes the initialisations to be done AFTER Guile is brought up, but
 * BEFORE the user's profile is read
 */

void GWM_init_guile()
{
  scm_c_eval_string("(define-module (guile-user) :use-module (ice-9 regex))");
  scm_c_eval_string("(define-module (guile-user) :use-module (ice-9 session))");
  scm_c_eval_string("(read-set! keywords 'prefix)");

  init_scm_error();
  init_scm_gwm();
  init_scm_deco();
  init_scm_client();
  init_scm_screen();
  init_scm_cursor();
  init_scm_drawing();
  init_scm_event();
  init_scm_fsm();
  init_scm_font();
  init_scm_active();
  init_scm_paint();
  init_scm_feedback();
  init_scm_decofunc();
  init_scm_gwmfunc();
  init_scm_wops();

  v_describe_window = scm_permanent_object(scm_c_define("describe-window", SCM_UNDEFINED));
  v_describe_icon = scm_permanent_object(scm_c_define("describe-icon", SCM_UNDEFINED));
  v_describe_screen = scm_permanent_object(scm_c_define("describe-screen", SCM_UNDEFINED));
  v_move_window = scm_c_lookup("move-window");
  v_resize_window = scm_c_lookup("resize-window");
  v_move_resize_window = scm_c_lookup("move-resize-window");
  v_raise_window = scm_c_lookup("raise-window");
  v_lower_window = scm_c_lookup("lower-window");
  v_iconify_window = scm_c_lookup("iconify-window");
  v_de_iconify_window = scm_c_lookup("deiconify-window");
  v_load_path = scm_c_lookup("%load-path");
  SCM_VARIABLE_SET(v_load_path, 
                   scm_parse_path(scm_makfrom0str(GWM_path), SCM_VARIABLE_REF(v_load_path)));
  
  DefaultClientClass = scm_makfrom0str("client");
  scm_gc_protect_object(DefaultClientClass);
  DefaultClientName = scm_makfrom0str("name");
  scm_gc_protect_object(DefaultClientName);
  DefaultWindowName = scm_makfrom0str("window");
  scm_gc_protect_object(DefaultWindowName);
  DefaultMachineName = scm_makfrom0str("machine");
  scm_gc_protect_object(DefaultMachineName);
  DefaultIconName = scm_makfrom0str("icon");
  scm_gc_protect_object(DefaultIconName);
}

void SetUpDefaults()
{
  DefaultFont = WlFont::Create(DEFAULT_FONT);
  if (DefaultFont == SCM_UNDEFINED) {
    gwm_puts("GWM: Cannot find default font ");
    gwm_puts(DEFAULT_FONT);
    gwm_puts(", aborting\n");
    gwm_end(1);
  } else {
    scm_gc_protect_object(DefaultFont);
  }

  /* X Atoms */
  XA_WM_STATE = XInternAtom(dpy, "WM_STATE", False);
  XA_WM_COLORMAP_WINDOWS = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", False);
  XA_WM_CHANGE_STATE = XInternAtom(dpy, "WM_CHANGE_STATE", False);
  XA_WM_PROTOCOLS = XInternAtom(dpy, "WM_PROTOCOLS", False);
  XA_WM_TAKE_FOCUS = XInternAtom(dpy, "WM_TAKE_FOCUS", False);
  XA_WM_SAVE_YOURSELF = XInternAtom(dpy, "WM_SAVE_YOURSELF", False);
  XA_WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
  XA_GNOME_SUPPORTING = XInternAtom(dpy, "_WIN_SUPPORTING_WM_CHECK", False);
  XA_GNOME_PROTOCOLS = XInternAtom(dpy, "_WIN_PROTOCOLS", False);
  XA_GWM_RUNNING = XInternAtom(dpy, "GWM_RUNNING", False);
}

void init_scm_gwmfunc()
{
#include "gwmfunc.x"
}
