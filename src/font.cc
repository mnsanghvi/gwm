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
 *    Font Object        *
 * 		         *
 \***********************/

#include <guile/gh.h>
#include <math.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "paint.hh"
#include "screen.hh"
#include "font.hh"

/*
extern long scm_tc16_wlfont;

class WlFont {
public:
  WlFont(char* str, XFontStruct* fs);
  ~WlFont();
  char* fontname() { return name; };
  long id() { return fontid; };
  void use_font(GC gc);
  void dimensions(int& x, int& y, int& w, int& h);
  void dimensions(char* str, int& x, int& y, int& w, int& h);
  Pixmap rotate_text(char* str, double ang, int mir, int& x, int& y, int& w, int& h, int onlydims = 0);
  static SCM Create(char* str);
  static SCM Find(char* str);
protected:
  char* name;
  long fontid;
  int ascent, descent, width, slant;
};

SCM wlfont2scm(WlFont* font);
SCM make_font(SCM string);

#define WLFONTP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlfont)
#define WL_FONT(x) ((WlFont*) SCM_CDR(x))

*/

long scm_tc16_wlfont;

SCM mark_wlfont(SCM obj)
{
  return SCM_BOOL_F;
}

size_t free_wlfont(SCM obj)
{
  delete WL_FONT(obj);
  return 0;
};

int print_wlfont(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<font: ", port);
  scm_puts(WL_FONT(obj)->fontname(), port);
  scm_puts(">", port);
  return 1;
};


SCM wlfont2scm(WlFont* font)
{
  return scm_cell((scm_t_bits) scm_tc16_wlfont, (scm_t_bits) font);
}

SCM_DEFINE(wl_font_p, "font?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a font.")
{
  return (WLFONTP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*
 * loading a font
 */

SCM_DEFINE(make_font, "make-font", 1, 0, 0,
           (SCM string),
           "Load the font with the name string.")
{
  SCM res;
  char* str;
  str = wl_getstring(string, s_make_font, 1);
  res = WlFont::Create(str);
  delete [] str;
  if (res == SCM_UNDEFINED) {
    gwm_warning("Cannot find font ~A , using default font", string);
    return DefaultFont;
  }
  return res;
}

WlFont::WlFont(char* str, XFontStruct* fs)
{
  int i, tmp, len;
  name = strcpy(new char[strlen(str)+1], str);
  fontid = fs->fid;
  ascent = fs->ascent;
  descent = fs->descent;
  width = fs->max_bounds.width;
  if (!fs->per_char || fs->min_byte1 || fs->max_byte1)
    slant = fs->max_bounds.rbearing-fs->min_bounds.lbearing-width; //guess slant
  else {
    slant = 0;
    len = fs->max_char_or_byte2 + 1 - fs->min_char_or_byte2;
    for (i=0; i<len; i++) {
      tmp = fs->per_char[i].rbearing - fs->per_char[i].lbearing - fs->per_char[i].width;
      if (tmp > slant) slant = tmp;
    }
  }
}

WlFont::~WlFont()
{
  XUnloadFont(dpy, fontid);
  delete [] name;
}

SCM WlFont::Create(char* str)
{
  char** ret;
  int num;
  XFontStruct* fp;
  SCM res;
  ret = XListFonts(dpy, str, 1, &num);
  if (num != 1)
    return SCM_UNDEFINED;
  fp = XLoadQueryFont(dpy, str);
  if (!fp)
    return SCM_UNDEFINED;
  res = wlfont2scm(new WlFont(ret[0], fp));
  XFreeFontInfo(ret, fp, 1);
  return res;
}

SCM WlFont::Find(char* str)
{
  char** ret;
  int num, i;
  SCM ele;
  SCM res = SCM_EOL;
  ret = XListFonts(dpy, str, 512, &num);
  for (i=0; i<num; i++) {
    ele = scm_makfrom0str(ret[i]);
    if (scm_member(ele, res) == SCM_BOOL_F)
      res = scm_cons(ele, res);
  }
  XFreeFontNames(ret);
  return scm_reverse_x(res, SCM_EOL);
}

void WlFont::use_font(GC gc)
{
  XSetFont(dpy, gc, fontid);
}

void WlFont::dimensions(int& x, int& y, int& w, int& h)
{
  x = slant;
  y = ascent;
  w = width;
  h = ascent + descent;
}

void WlFont::dimensions(char* str, int& x, int& y, int& w, int& h)
{
  int up, down, dir;
  XCharStruct extent;
  int len = strlen(str);
  XQueryTextExtents(dpy, fontid, (len ? str : " "), (len ? len : 1),
                    &dir, &up, &down, &extent);
  x = - extent.lbearing;
  y = up;
  w = (len ? extent.width : 0);
  h = up + down;
}

Pixmap WlFont::rotate_text(char* str, double ang, int mir, int& x, int& y, int& w, int& h, int onlydims)
{
  static Pixmap opix = 0;
  static Pixmap npix = 0;
  static int pow = 0, poh = 0;
  static int pnw = 0, pnh = 0;
  int ox, oy, ow, oh;
  int nx, ny, nw, nh;
  float xdx, xdy, ydx, ydy, cx, cy;
  float r, l, t, b;
  int ix, iy, jx, jy;
  XImage *oimage, *nimage;

  dimensions(str, ox, oy, ow, oh);
  if (mir) {
    ydy = cos(ang * M_PI / 180.0);
    xdx = -ydy;
    ydx = xdy = -sin(ang * M_PI / 180.0);
  } else {
    ydy = xdx = cos(ang * M_PI / 180.0);
    xdy = -sin(ang * M_PI / 180.0);
    ydx = -xdy;
  }
  cx = xdx * -ox + xdy * -oy, cy = ydx * -ox + ydy * -oy;
  l = r = cx, t = b = cy;
  cx = xdx * -ox + xdy * (oh-oy), cy = ydx * -ox + ydy * (oh-oy);
  l = (cx<l ? cx : l), r = (cx>r ? cx : r), t = (cy<t ? cy : t), b = (cy>b ? cy : b);
  cx = xdx * (ow-ox) + xdy * -oy, cy = ydx * (ow-ox) + ydy * -oy;
  l = (cx<l ? cx : l), r = (cx>r ? cx : r), t = (cy<t ? cy : t), b = (cy>b ? cy : b);
  cx = xdx * (ow-ox) + xdy * (oh-oy), cy = ydx * (ow-ox) + ydy * (oh-oy);
  l = (cx<l ? cx : l), r = (cx>r ? cx : r), t = (cy<t ? cy : t), b = (cy>b ? cy : b);
  nx = -(int)floor(l), ny = -(int)floor(t);
  nw = nx + (int)ceil(r), nh = ny + (int)ceil(b);
  x = nx, y = ny, w = nw, h = nh;
  if (onlydims)
    return 0;
  if (!opix || pow < ow || poh < oh) {
    if (opix)
      XFreePixmap(dpy, opix);
    pow = (pow <= 256 ? Max(pow, ow) : ow);
    poh = (poh <= 256 ? Max(poh, oh) : oh);
    opix = XCreatePixmap(dpy, Context->Root(), pow, poh, 1);
  }
  if (!npix || pnw < nw || pnh < nh) {
    if (npix)
      XFreePixmap(dpy, npix);
    pnw = (pnw <= 256 ? Max(pnw, nw) : nw);
    pnh = (pnh <= 256 ? Max(pnh, nh) : nh);
    npix = XCreatePixmap(dpy, Context->Root(), pnw, pnh, 1);
  }
  XSetForeground(dpy, Context->gc.Shape, 0);
  XFillRectangle(dpy, npix, Context->gc.Shape, 0, 0, pnw, pnh);
  XFillRectangle(dpy, opix, Context->gc.Shape, 0, 0, pow, poh);
  nimage = XGetImage(dpy, npix, 0, 0, nw, nh, 1, XYPixmap);
  XSetForeground(dpy, Context->gc.Shape, 1);
  use_font(Context->gc.Shape);
  XDrawString(dpy, opix, Context->gc.Shape, ox, oy, str, strlen(str));
  oimage = XGetImage(dpy, opix, 0, 0, ow, oh, 1, XYPixmap);
  for (ix = 0; ix < nw; ix++)
    for (iy = 0; iy < nh; iy++) {
      cx = xdx * (ix-nx+0.5) + ydx * (iy-ny+0.5) + ox;
      cy = xdy * (ix-nx+0.5) + ydy * (iy-ny+0.5) + oy;
      jx = (int)floor(cx), jy = (int)floor(cy);
      if (jx >= 0 && jx < ow && jy >= 0 && jy < oh)
        if (XGetPixel(oimage, jx, jy))
          XPutPixel(nimage, ix, iy, 1);
    }
  XPutImage(dpy, npix, Context->gc.Shape, nimage, 0, 0, 0, 0, nw, nh);
  XDestroyImage(oimage);
  XDestroyImage(nimage);
  return npix;
}

SCM_DEFINE(font_name, "font-name", 1, 0, 0,
           (SCM font),
           "Get the name of the font.")
{
  if (!WLFONTP(font))
    gwm_wrong_type_arg(s_font_name, 1, font, "font");
  return scm_makfrom0str(WL_FONT(font)->fontname());
}

SCM_DEFINE(font_dimensions, "font-dimensions", 1, 0, 0,
           (SCM font),
           "Get the width, height, (approximate) slant and ascent of the font.")
{
  int x, y, w, h;
  SCM result;
  if (!WLFONTP(font))
    gwm_wrong_type_arg(s_font_dimensions, 1, font, "font");
  WL_FONT(font)->dimensions(x, y, w, h);
  result = gh_list(gh_int2scm(w), gh_int2scm(h), gh_int2scm(x), gh_int2scm(y),
                   SCM_UNDEFINED);
  return result;
}

SCM_DEFINE(string_dimensions, "string-dimensions", 2, 0, 1,
           (SCM object, SCM font, SCM args),
           "Get the width, height, left bearing and ascent of the string when using"
           "the font. Possible keys are:"
           ":angle           angle of text, clockwise in degrees"
           ":mirrored        mirror-reverse text if true")
{
  int x, y, w, h;
  SCM result;
  SCM sub, ctx;
  int n, cn;
  SCM ang, mir;
  char* str;
  n = wl_separate_context(args, sub, ctx, cn, s_string_dimensions);
  if (n != 0)
    gwm_wrong_num_args(s_string_dimensions, n+2);
  if (!WLFONTP(font))
    gwm_wrong_type_arg(s_string_dimensions, 2, font, "font");
  ang = gwm_get_keyword(k_angle, ctx, cn, gh_double2scm(0.0));
  if (!gh_number_p(ang))
    gwm_wrong_type_arg(s_string_dimensions, 0, ang, "real number");
  mir = gwm_get_keyword(k_mirrored, ctx, cn, SCM_BOOL_F);
  str = wl_getstring(object, s_string_dimensions, 1);
  if (mir != SCM_BOOL_F || gh_scm2double(ang) != 0.0)
    WL_FONT(font)->rotate_text(str, gh_scm2double(ang), 
                               (mir != SCM_BOOL_F ? 1 : 0), x, y, w, h, 1);
  else
    WL_FONT(font)->dimensions(str, x, y, w, h);
  delete [] str;
  result = gh_list(gh_int2scm(w), gh_int2scm(h), gh_int2scm(x), gh_int2scm(y),
                   SCM_UNDEFINED);
  return result;
}

SCM_DEFINE(find_fonts, "find-fonts", 1, 0, 0,
           (SCM string),
           "Return a list of possible expansions for the font name string.")
{
  SCM res;
  char* str;
  str = wl_getstring(string, s_make_font, 1);
  res = WlFont::Find(str);
  delete [] str;
  return res;
}

void init_scm_font()
{
  scm_tc16_wlfont = scm_make_smob_type("font", 0);
  scm_set_smob_mark(scm_tc16_wlfont, mark_wlfont);
  scm_set_smob_free(scm_tc16_wlfont, free_wlfont);
  scm_set_smob_print(scm_tc16_wlfont, print_wlfont);
#include "font.x"
}
