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


 /***********************\
 * 		         *
 *  Cursor Object        *
 * 		         *
 \***********************/

#include <guile/gh.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "paint.hh"
#include "screen.hh"
#include "cursor.hh"

/*
extern long scm_tc16_wlcursor;

class WlCursor {
public:
    WlCursor(int num, XColor* cols);
    WlCursor(Pixmap pix, Pixmap mask, int w, int h, int x, int y, XColor* cols);
    ~WlCursor();
    void use_cursor(Window w);
    static void use_no_cursor(Window w);
    Cursor cursor;    
};

SCM make_cursor(SCM arg1, SCM arg2);

#define WLCURSORP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlcursor)
#define WL_CURSOR(x) ((WlCursor*) SCM_CDR(x))
*/

long scm_tc16_wlcursor;

SCM mark_wlcursor(SCM obj)
{
  return SCM_BOOL_F;
}

size_t free_wlcursor(SCM obj)
{
  delete WL_CURSOR(obj);
  return 0;
};

int print_wlcursor(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<cursor>", port);
  return 1;
};


SCM wlcursor2scm(WlCursor* cursor)
{
  return scm_cell((scm_t_bits) scm_tc16_wlcursor, (scm_t_bits) cursor);
}

SCM_DEFINE(wl_cursor_p, "cursor?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a cursor.")
{
  return (WLCURSORP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


/*
 * Constructor: make_cursor
 *   arguments: bitmap & mask
 * font cursor code contributed by Bill Trost <trost@scott.labs.tek.com>
 */

SCM_DEFINE(make_cursor, "make-cursor", 1, 0, 1,
           (SCM arg1, SCM args),
           "(make-cursor number [key val] ...)"
           "(make-cursor filename [filename2] [key val] ...)"
           "Make a cursor, either from a number or from one or two bitmap files. Keys can be:"
           ":foreground      foreground color of cursor"
           ":background      background color of cursor")
{
    WlCursor* object;
    XColor colors[2];
    Pixmap bitmap, mask;
    SCM sub, ctx;
    SCM fg, bg;
    char* str;
    int cursor_number;
    int use_cursor_font = 0;
    int n, cn, slen;
    int x, y, w, h;

    if ((n = wl_separate_context(args, sub, ctx, cn, s_make_cursor)) > 1)
      gwm_wrong_num_args(s_make_cursor, n+1);
    else if (n == 1) {
        must_be_string(s_make_cursor, arg1, 1);
        must_be_string(s_make_cursor, SCM_CAR(sub), 2);
	mask = gwm_raw_bitmap_load(SCM_CAR(sub), &w, &h, &x, &y);
	bitmap = gwm_raw_bitmap_load(arg1, &w, &h, &x, &y);
    } else {
	if (scm_is_integer(arg1)) {
	    use_cursor_font = 1;
	    cursor_number = gh_scm2int(arg1);
	} else {
	    must_be_string(s_make_cursor, arg1, 1);
            slen = scm_i_string_length(arg1);
	    str = new char[slen + 3];
	    strncpy(str, scm_i_string_chars(arg1), slen);
	    strcpy(str+slen, "-m");
	    mask = gwm_raw_bitmap_load(scm_makfrom0str(str), &w, &h, &x, &y);
	    strcpy(str+slen, "-f");
	    bitmap = gwm_raw_bitmap_load(scm_makfrom0str(str), &w, &h, &x, &y);
            delete [] str;
	}
    }
    fg = gwm_get_keyword(k_foreground, ctx, cn, Context->pixel.Black);
    if (!WLCOLORP(fg))
      gwm_wrong_type_arg(s_make_cursor, 0, fg, "color");
    bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
    if (!WLCOLORP(bg))
      gwm_wrong_type_arg(s_make_cursor, 0, bg, "color");
    colors[0].pixel = WL_PAINT(fg)->Pixel();
    colors[1].pixel = WL_PAINT(bg)->Pixel();
    XQueryColors(dpy, DefaultColormap(dpy, Context->ScreenNum()), colors, 2);

    if (use_cursor_font)
      object = new WlCursor(cursor_number, colors);
    else
      object = new WlCursor(bitmap, mask, w, h, x, y, colors);
    return wlcursor2scm(object);
}

WlCursor::WlCursor(int num, XColor* cols)
{
  cursor = XCreateFontCursor(dpy, num);
  XRecolorCursor(dpy, cursor, &cols[0], &cols[1]);
}

WlCursor::WlCursor(Pixmap pix, Pixmap mask, int w, int h, int x, int y, XColor* cols)
{
  if (x < 0 || x >= w)
    x = 0;
  if (y < 0 || y >= h)
    y = 0;
  cursor = XCreatePixmapCursor(dpy, pix, mask, &cols[0], &cols[1], x, y);
}

WlCursor::~WlCursor()
{
  XFreeCursor(dpy, cursor);
}

void WlCursor::use_cursor(Window w)
{
  XSetWindowAttributes wa;
  wa.cursor = cursor;
  XChangeWindowAttributes(dpy, w, CWCursor, &wa);
}

void WlCursor::use_no_cursor(Window w)
{
  XSetWindowAttributes wa;
  wa.cursor = None;
  XChangeWindowAttributes(dpy, w, CWCursor, &wa);
}

void init_scm_cursor()
{
  scm_tc16_wlcursor = scm_make_smob_type("cursor", 0);
  scm_set_smob_mark(scm_tc16_wlcursor, mark_wlcursor);
  scm_set_smob_free(scm_tc16_wlcursor, free_wlcursor);
  scm_set_smob_print(scm_tc16_wlcursor, print_wlcursor);
#include "cursor.x"
}
