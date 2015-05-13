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
 *    Paint Object     *
 * 		       *
 \*********************/

#include <guile/gh.h>
#include <stdio.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "paint.hh"
#include "font.hh"
#include "cursor.hh"
#include "deco.hh"
#include "screen.hh"
#include "active.hh"
#include "drawing.hh"
#include "client.hh"
#include "event.hh"

#include "def_bitmap.h"
#ifdef HAVE_IMLIB_2
#include <Imlib2.h>
#elif defined HAVE_IMLIB
#include <Imlib.h>
#endif

/*
extern long scm_tc16_wlcolor;
extern long scm_tc16_wlpixmap;

class Paint {
  friend class WlColor;
  friend class Transparent;
  friend class WlPixmap;
public:
  virtual ~Paint();
  virtual int print(SCM port) = 0;
  virtual long Pixel();
  virtual void freepixel() {};
  virtual SCM components();
  virtual int IsShapedT() = 0;
  virtual int IsShapedH() = 0;
  virtual void paint_border(Window hook, struct Box* box) = 0;
  virtual void paint_background(Window hook, struct Box* box) = 0;
  virtual void paint_border_shape(Window hook, struct Box* box) = 0;
  virtual void paint_background_shape(Window hook, struct Box* box) = 0;
  virtual void paint_border_hole(Window hook, struct Box* box, int xoff, int yoff) = 0;
  virtual void paint_background_hole(Window hook, struct Box* box, int xoff, int yoff) = 0;
  virtual void draw_point(class WlPixmap* dest, int x, int y) = 0;
  virtual void draw_point(Window win, int x, int y) = 0;
  virtual void draw_point_sh(class WlActive* dest, int x, int y) = 0;
  virtual void draw_line(class WlPixmap* dest, int x1, int y1, int x2, int y2) = 0;
  virtual void draw_line(Window win, int x1, int y1, int x2, int y2) = 0;
  virtual void draw_line_sh(class WlActive* dest, int x1, int y1, int x2, int y2) = 0;
  virtual void draw_polygon(class WlPixmap* dest, int nump, XPoint* points) = 0;
  virtual void draw_polygon(Window win, int nump, XPoint* points) = 0;
  virtual void draw_polygon_sh(class WlActive* dest, int nump, XPoint* points) = 0;
  virtual void draw_rect(class WlPixmap* dest, int x, int y, int w, int h, int bord) = 0;
  virtual void draw_rect(Window win, int x, int y, int w, int h, int bord) = 0;
  virtual void draw_rect_sh(class WlActive* dest, int x, int y, int w, int h, int bord) = 0;
  virtual void draw_circle(class WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040) = 0;
  virtual void draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040) = 0;
  virtual void draw_circle_sh(class WlActive* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040) = 0;
  virtual void draw_text(class WlPixmap* dest, int x, int y, class WlFont* font, char* txt) = 0;
  virtual void draw_text(Window win, int x, int y, class WlFont* font, char* txt) = 0;
  virtual void draw_text_sh(class WlActive* dest, int x, int y, class WlFont* font, char* txt) = 0;
  virtual void draw_bitmap(class WlPixmap* dest, int x, int y, int w, int h, Pixmap mask) = 0;
  virtual void draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask) = 0;
  virtual void draw_bitmap_sh(class WlActive* dest, int x, int y, int w, int h, Pixmap mask) = 0;
protected:
  void shape_border(Window hook, struct Box* box, int xoff, int yoff, int sub);
  void shape_background(Window hook, struct Box* box, int xoff, int yoff, int sub);
  void shape_border_pixmap(Window hook, struct Box* box, int xoff, int yoff,
                           Pixmap pm, Pixmap pm2, int pwdt, int phgt);
  void shape_background_pixmap(Window hook, struct Box* box, int xoff, int yoff,
                               Pixmap pm, Pixmap pm2, int pwdt, int phgt);
  void shape_rect_pixmap(Window hook, int x, int y, int wdt, int hgt, 
                         int xoff, int yoff, Pixmap pm, Pixmap pm2, int pwdt, int phgt);
};

class WlColor : public Paint {
public:
  WlColor(char* str, long col);
  virtual ~WlColor();
  virtual int print(SCM port);
  virtual long Pixel() { return pixel; };
  virtual void freepixel();
  virtual SCM components();
  virtual int IsShapedT() { return 0; };
  virtual int IsShapedH() { return 0; };
  virtual void paint_border(Window hook, struct Box* box);
  virtual void paint_background(Window hook, struct Box* box);
  virtual void paint_border_shape(Window hook, struct Box* box);
  virtual void paint_background_shape(Window hook, struct Box* box);
  virtual void paint_border_hole(Window hook, struct Box* box, int xoff, int yoff);
  virtual void paint_background_hole(Window hook, struct Box* box, int xoff, int yoff);
  virtual void draw_point(class WlPixmap* dest, int x, int y);
  virtual void draw_point(Window win, int x, int y);
  virtual void draw_point_sh(class WlActive* dest, int x, int y);
  virtual void draw_line(class WlPixmap* dest, int x1, int y1, int x2, int y2);
  virtual void draw_line(Window win, int x1, int y1, int x2, int y2);
  virtual void draw_line_sh(class WlActive* dest, int x1, int y1, int x2, int y2);
  virtual void draw_polygon(class WlPixmap* dest, int nump, XPoint* points);
  virtual void draw_polygon(Window win, int nump, XPoint* points);
  virtual void draw_polygon_sh(class WlActive* dest, int nump, XPoint* points);
  virtual void draw_rect(class WlPixmap* dest, int x, int y, int w, int h, int bord);
  virtual void draw_rect(Window win, int x, int y, int w, int h, int bord);
  virtual void draw_rect_sh(class WlActive* dest, int x, int y, int w, int h, int bord);
  virtual void draw_circle(class WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_circle_sh(class WlActive* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_text(class WlPixmap* dest, int x, int y, class WlFont* font, char* txt);
  virtual void draw_text(Window win, int x, int y, class WlFont* font, char* txt);
  virtual void draw_text_sh(class WlActive* dest, int x, int y, class WlFont* font, char* txt);
  virtual void draw_bitmap(class WlPixmap* dest, int x, int y, int w, int h, Pixmap mask);
  virtual void draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask);
  virtual void draw_bitmap_sh(class WlActive* dest, int x, int y, int w, int h, Pixmap mask);
  static SCM Create(char* str);
  static SCM Create(int r, int g, int b);
protected:
//  void init(int screen);
  char* colname;
  long pixel;
//  int screen;
//  XColor xcol;
//  long* cols;
//  char* flags;
};

class Transparent : public Paint {
public:
  Transparent(int h);
  virtual ~Transparent();
  virtual int print(SCM port);
  virtual int IsShapedT() { return !hole; };
  virtual int IsShapedH() { return hole; };
  virtual void paint_border(Window hook, struct Box* box);
  virtual void paint_background(Window hook, struct Box* box);
  virtual void paint_border_shape(Window hook, struct Box* box);
  virtual void paint_background_shape(Window hook, struct Box* box);
  virtual void paint_border_hole(Window hook, struct Box* box, int xoff, int yoff);
  virtual void paint_background_hole(Window hook, struct Box* box, int xoff, int yoff);
  virtual void draw_point(class WlPixmap* dest, int x, int y);
  virtual void draw_point(Window win, int x, int y);
  virtual void draw_point_sh(class WlActive* dest, int x, int y);
  virtual void draw_line(class WlPixmap* dest, int x1, int y1, int x2, int y2);
  virtual void draw_line(Window win, int x1, int y1, int x2, int y2);
  virtual void draw_line_sh(class WlActive* dest, int x1, int y1, int x2, int y2);
  virtual void draw_polygon(class WlPixmap* dest, int nump, XPoint* points);
  virtual void draw_polygon(Window win, int nump, XPoint* points);
  virtual void draw_polygon_sh(class WlActive* dest, int nump, XPoint* points);
  virtual void draw_rect(class WlPixmap* dest, int x, int y, int w, int h, int bord);
  virtual void draw_rect(Window win, int x, int y, int w, int h, int bord);
  virtual void draw_rect_sh(class WlActive* dest, int x, int y, int w, int h, int bord);
  virtual void draw_circle(class WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_circle_sh(class WlActive* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_text(class WlPixmap* dest, int x, int y, class WlFont* font, char* txt);
  virtual void draw_text(Window win, int x, int y, class WlFont* font, char* txt);
  virtual void draw_text_sh(class WlActive* dest, int x, int y, class WlFont* font, char* txt);
  virtual void draw_bitmap(class WlPixmap* dest, int x, int y, int w, int h, Pixmap mask);
  virtual void draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask);
  virtual void draw_bitmap_sh(class WlActive* dest, int x, int y, int w, int h, Pixmap mask);
protected:
  int hole;
};

class WlPixmap : public Paint {
  friend class WlColor;
  friend class Transparent;
public:
  WlPixmap(int w, int h);
  WlPixmap(int w, int h, Pixmap pix, Pixmap shape, int hole);
  virtual ~WlPixmap();
  virtual int print(SCM port);
  virtual int IsShapedT() { return (transmask ? 1 : 0); };
  virtual int IsShapedH() { return (holemask ? 1 : 0); };
  void dimensions(struct Box* box);
  virtual void paint_border(Window hook, struct Box* box);
  virtual void paint_background(Window hook, struct Box* box);
  virtual void paint_border_shape(Window hook, struct Box* box);
  virtual void paint_background_shape(Window hook, struct Box* box);
  virtual void paint_border_hole(Window hook, struct Box* box, int xoff, int yoff);
  virtual void paint_background_hole(Window hook, struct Box* box, int xoff, int yoff);
  virtual void draw_point(class WlPixmap* dest, int x, int y);
  virtual void draw_point(Window win, int x, int y);
  virtual void draw_point_sh(class WlActive* dest, int x, int y);
  virtual void draw_line(class WlPixmap* dest, int x1, int y1, int x2, int y2);
  virtual void draw_line(Window win, int x1, int y1, int x2, int y2);
  virtual void draw_line_sh(class WlActive* dest, int x1, int y1, int x2, int y2);
  virtual void draw_polygon(class WlPixmap* dest, int nump, XPoint* points);
  virtual void draw_polygon(Window win, int nump, XPoint* points);
  virtual void draw_polygon_sh(class WlActive* dest, int nump, XPoint* points);
  virtual void draw_rect(class WlPixmap* dest, int x, int y, int w, int h, int bord);
  virtual void draw_rect(Window win, int x, int y, int w, int h, int bord);
  virtual void draw_rect_sh(class WlActive* dest, int x, int y, int w, int h, int bord);
  virtual void draw_circle(class WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_circle_sh(class WlActive* dest, int x, int y, int w, int h, int bord, int ang1 = 0, int ang2 = 23040);
  virtual void draw_text(class WlPixmap* dest, int x, int y, class WlFont* font, char* txt);
  virtual void draw_text(Window win, int x, int y, class WlFont* font, char* txt);
  virtual void draw_text_sh(class WlActive* dest, int x, int y, class WlFont* font, char* txt);
  virtual void draw_bitmap(class WlPixmap* dest, int x, int y, int w, int h, Pixmap mask);
  virtual void draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask);
  virtual void draw_bitmap_sh(class WlActive* dest, int x, int y, int w, int h, Pixmap mask);
  void setup_shape(int hole);
  void register_use(class Decoration* deco, int flag);
  void unregister_use(class Decoration* deco, int flag);
  void issue_expose_users();
protected:
  int x_hot, y_hot;
  unsigned int width, height;
  Pixmap pixmap;
  Pixmap transmask;
  Pixmap holemask;
  class DecoListP* usedby;
};

Pixmap MakeDefaultBitmap(Window root);
Pixmap gwm_raw_bitmap_load(SCM filename, int* width, int* height, int* x_hot, int* y_hot);
SCM gwm_pixmap_from_pixmap(Pixmap pixmap, Pixmap shape, SCM fg, SCM bg, int hole);
SCM color2scm(Paint* paint);
SCM pixmap2scm(Paint* paint);

#define WLPAINTP(x) (SCM_NIMP(x) && (SCM_CAR(x) == (SCM)scm_tc16_wlpixmap || SCM_CAR(x) == (SCM)scm_tc16_wlcolor))
#define WL_PAINT(x) ((Paint*) SCM_CDR(x))
#define WLCOLORP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlcolor)
// #define WL_COLOR(x) ((WlColor*) SCM_CDR(x))
#define WLPIXMAPP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlpixmap)
#define WL_PIXMAP(x) ((WlPixmap*) SCM_CDR(x))
*/ 

#define BITMAP_EXTENSION ".xbm"
#define PIXMAP_EXTENSION ".xpm"

long scm_tc16_wlcolor;
long scm_tc16_wlpixmap;

SCM mark_paint(SCM obj)
{
  return SCM_BOOL_F;
}

size_t free_paint(SCM obj)
{
  delete WL_PAINT(obj);
  return 0;
}

int print_paint(SCM obj, SCM port, scm_print_state * pstate)
{
  return WL_PAINT(obj)->print(port);
}

int WlColor::print(SCM port)
{
  scm_puts("#<color: ", port);
  scm_puts(colname, port);
  scm_puts(">", port);
  return 1;
}

int Transparent::print(SCM port)
{
  if (hole)
    scm_puts("#<color: hole>", port);
  else
    scm_puts("#<color: transparent>", port);
  return 1;
}

int WlPixmap::print(SCM port)
{
  scm_puts("#<pixmap: ", port);
  scm_write(gh_int2scm(width), port);
  scm_puts("x", port);
  scm_write(gh_int2scm(height), port);
  scm_puts(">", port);
  return 1;
}

SCM color2scm(Paint* paint)
{
  return scm_cell((scm_t_bits) scm_tc16_wlcolor, (scm_t_bits) paint);
}

SCM pixmap2scm(Paint* paint)
{
  return scm_cell((scm_t_bits) scm_tc16_wlpixmap, (scm_t_bits) paint);
}

SCM_DEFINE(wl_color_p, "color?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a color.")
{
  return (WLCOLORP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM_DEFINE(wl_pixmap_p, "pixmap?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a pixmap.")
{
  return (WLPIXMAPP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

Paint::~Paint()
{
}

WlColor::WlColor(char* str, long col)
{
  colname = strcpy(new char[strlen(str)+1], str);
  pixel = col;
}

WlColor::~WlColor()
{
  delete [] colname;
}

/*
 * Returns a color (pixel value) from an English name, or a #RGB description
 */

SCM WlColor::Create(char* str)
{
  WlColor* col;
  XColor x_color;
  XSync(dpy, 0);
  XSetErrorHandler(NoXError);
  if (!XParseColor(dpy, DefaultColormap(dpy, Context->ScreenNum()),
                   str, &x_color)) {
    gwm_warning("Color not found: ~A\n", scm_makfrom0str(str));
    XSync(dpy, 0);
    XSetErrorHandler(XError);
    return Context->pixel.White;
  }
  if (!XAllocColor(dpy, DefaultColormap(dpy, Context->ScreenNum()), &x_color)) {
    gwm_warning("Colormap full, couldn't allocate Color: ~A\n", scm_makfrom0str(str));
    XSync(dpy, 0);
    XSetErrorHandler(XError);
    return Context->pixel.White;
  }
  XSync(dpy, 0);
  XSetErrorHandler(XError);
  col = new WlColor(str, x_color.pixel);
  return color2scm(col);
}

SCM WlColor::Create(int r, int g, int b)
{
  WlColor* col;
  XColor x_color;
  char *buf;
  x_color.red = r = (r < 0 ? 0 : (r > 65535 ? 65535 : r));
  x_color.green = g = (g < 0 ? 0 : (g > 65535 ? 65535 : g));
  x_color.blue = b = (b < 0 ? 0 : (b > 65535 ? 65535 : b));
  buf = new char[16];
  sprintf(buf, "%04x%04x%04x", r, g, b);
  XSync(dpy, 0);
  XSetErrorHandler(NoXError);
  if (!XAllocColor(dpy, DefaultColormap(dpy, Context->ScreenNum()), &x_color)) {
    gwm_warning("Colormap full, couldn't allocate Color: ~A\n", scm_makfrom0str(buf));
    XSync(dpy, 0);
    XSetErrorHandler(XError);
    delete [] buf;
    return Context->pixel.White;
  }
  XSync(dpy, 0);
  XSetErrorHandler(XError);
  col = new WlColor(buf, x_color.pixel);
  delete [] buf;
  return color2scm(col);
}

Transparent::Transparent(int h)
{
  hole = h;
}

Transparent::~Transparent()
{
}

SCM_DEFINE(make_color, "make-color", 1, 2, 0,
           (SCM arg1, SCM arg2, SCM arg3),
           "(make-color name)"
           "(make-color r g b)"
           "Make a color from name, possibly 'transparent or 'hole, or from the"
           "rgb-components of the color.")
{
  if (arg2 != SCM_UNDEFINED) {
    if (arg3 == SCM_UNDEFINED)
      gwm_wrong_num_args(s_make_color, 2);
    must_be_number(s_make_color, arg1, 1);
    must_be_number(s_make_color, arg2, 2);
    must_be_number(s_make_color, arg3, 3);
    return WlColor::Create(gh_scm2int(arg1),
                           gh_scm2int(arg2),
                           gh_scm2int(arg3));
  } else if (arg1 == WA_transparent) {
    return color2scm(new Transparent(0));
  } else if (arg1 == WA_hole) {
    return color2scm(new Transparent(1));
  } else {
    char* str = wl_getstring(arg1, s_make_color, 1);
    SCM ret = WlColor::Create(str);
    delete [] str;
    return ret;
  }
}

long Paint::Pixel()
{
  return WL_PAINT(Context->pixel.Black)->Pixel();
}

SCM_DEFINE(free_color, "free-color", 0, 0, 1,
           (SCM args),
           "Free the given colors from the colormap. Use with care, things may look"
           "strange if the colors are still in use.")
{
  int i, argc;
  argc = scm_ilength(args);
  for (i = 0; i < argc; i++) {
    if (!WLCOLORP(SCM_CAR(args)))
      gwm_wrong_type_arg(s_free_color, i+1, SCM_CAR(args), "color");
    WL_PAINT(SCM_CAR(args))->freepixel();
    args = SCM_CDR(args);
  }
  return SCM_UNSPECIFIED;
}

void WlColor::freepixel()
{
  unsigned long pixel;
  XSync(dpy, 0);
  XSetErrorHandler(NoXError);
  XFreeColors(dpy, DefaultColormap(dpy, Context->ScreenNum()),
              &pixel, 1, 0);
  XSync(dpy, 0);
  XSetErrorHandler(XError);
}

/* returns the RGB values of a color as a list of 3 integers between
 * 0 and 65535
 */

SCM_DEFINE(color_components, "color-components", 1, 0, 0,
           (SCM color),
           "Get the rgb-components of color.")
{
  if (!WLCOLORP(color))
    gwm_wrong_type_arg(s_free_color, 1, color, "color");
  return WL_PAINT(color)->components();
}

SCM WlColor::components()
{
  XColor x_color;
  SCM result;
  x_color.pixel = pixel;
  XQueryColor(dpy, DefaultColormap(dpy, Context->ScreenNum()), &x_color);
  result = gh_list(gh_int2scm(x_color.red),
                   gh_int2scm(x_color.green),
                   gh_int2scm(x_color.blue),
                   SCM_UNDEFINED);
  return result;
}

SCM Paint::components()
{
  return WL_PAINT(Context->pixel.Black)->components();
}

class DecoListP {
public:
  DecoListP(Decoration* d, int f) { deco = d; flag = f; next = 0; };
  Decoration* deco;
  int flag;
  DecoListP* next;
};

WlPixmap::WlPixmap(int w, int h)
{
  x_hot = 0;
  y_hot = 0;
  width = w;
  height = h;
  usedby = 0;
  pixmap = XCreatePixmap(dpy, Context->Root(), w, h, Context->depth);
  transmask = 0;
  holemask = 0;
}

WlPixmap::WlPixmap(int w, int h, Pixmap pix, Pixmap shape, int hole)
{
  x_hot = 0;
  y_hot = 0;
  width = w;
  height = h;
  usedby = 0;
  pixmap = pix;
  if (shape) {
    XGCValues gcvals;
    gcvals.function = GXinvert;
    XChangeGC(dpy, Context->gc.Shape, GCFunction, &gcvals);
    XFillRectangle(dpy, shape, Context->gc.Shape, 0, 0, w, h);
    gcvals.function = GXcopy;
    XChangeGC(dpy, Context->gc.Shape, GCFunction, &gcvals);
    transmask = (hole ? 0 : shape);
    holemask = (hole ? shape : 0);
  } else {
    transmask = 0;
    holemask = 0;
  }
}

WlPixmap::~WlPixmap()
{
  DecoListP* p;
  XFreePixmap(dpy, pixmap);
  if (transmask)
    XFreePixmap(dpy, transmask);
  if (holemask)
    XFreePixmap(dpy, holemask);
  while (usedby) {
    p = usedby;
    usedby = usedby->next;
    delete p;
  }
}

void WlPixmap::dimensions(Box* box)
{
  box->x = -x_hot;
  box->y = -y_hot;
  box->width = width;
  box->height = height;
  box->borderwidth = 0;
  box->shape_flags = 0;
}

void WlPixmap::register_use(class Decoration* deco, int flag)
{
  DecoListP* ele = new DecoListP(deco, flag);
  ele->next = usedby;
  usedby = ele;
}

void WlPixmap::unregister_use(class Decoration* deco, int flag)
{
  DecoListP *ele = usedby, *ele2 = 0;
  while (ele && (ele->deco != deco || ele->flag != flag))
    ele2 = ele, ele = ele->next;
  if (ele) {
    if (ele2)
      ele2->next = ele->next;
    else
      usedby = ele->next;
    delete ele;
  }
}

void WlPixmap::issue_expose_users()
{
  DecoListP *ele = usedby;
  while (ele) {
    if (ele->deco->Valid() > 0)
      send_gwm_event(ele->deco, (ele->flag ? GWMExposeBordEvent : GWMExposeEvent));
    ele = ele->next;
  }
}

SCM make_default_pixmap(SCM fg, SCM bg)
{
  WlPixmap* obj = new WlPixmap(def_bitmap_width, def_bitmap_height);
  WL_PAINT(bg)->draw_rect(obj, 0, 0, def_bitmap_width, def_bitmap_height, 0);
  WL_PAINT(fg)->draw_bitmap(obj, 0, 0, def_bitmap_width, def_bitmap_height, Context->DefaultBitmap);
  return pixmap2scm(obj);
}

Pixmap gwm_raw_bitmap_load(SCM filename, int* width, int* height, int* x_hot, int* y_hot)
{
  SCM name;
  Pixmap ret;
  char* str;
  name = scm_search_path(SCM_VARIABLE_REF(v_load_path),
                         filename,
                         scm_listify(scm_makfrom0str(""),
                                     scm_makfrom0str(BITMAP_EXTENSION),
                                     SCM_UNDEFINED));
  if (name && name != SCM_BOOL_F) {
    str = wl_getstring(name, "");
    switch (XReadBitmapFile(dpy, Context->Root(), str,
                            (unsigned int*) width, (unsigned int*) height,
                            &ret, x_hot, y_hot)) {
    case BitmapOpenFailed:
      gwm_warning("Cannot open file ~A, using default instead", name);
      break;
    case BitmapFileInvalid:
      gwm_warning("File ~A is not a bitmap, using default instead", name);
      break;
    case BitmapNoMemory:
      scm_memory_error(0);
      break;
    default:
      delete [] str;
      return ret;
    }
    delete [] str;
  } else {
    gwm_warning("Cannot find bitmap ~A, using default instead", filename);
  }
  ret = Context->DefaultBitmap;
  *width = def_bitmap_width;
  *height = def_bitmap_height;
  *x_hot = def_bitmap_x_hot;
  *y_hot = def_bitmap_y_hot;
  return ret;
}

SCM gwm_pixmap_from_pixmap(Pixmap pixmap, Pixmap shape, SCM fg, SCM bg, int hole)
{
  WlPixmap* object;
  Pixmap respix, resshape;
  unsigned int bw, depth, width, height;
  int x_hot, y_hot;
  Window root;
  
  XGetGeometry(dpy, pixmap, &root, &x_hot, &y_hot,
               &width, &height, &bw, &depth);
  
  if (!width || !height || !depth) {	/* ERROR */
    gwm_warning("Invalid pixmap size, using default instead", 0);
    return make_default_pixmap(fg, bg);
  } else if (depth == 1) {	/* BITMAP */
    if (shape) {
      respix = XCreatePixmap(dpy, Context->Root(), width, height, Context->depth);
      resshape = XCreatePixmap(dpy, Context->Root(), width, height, 1);
      XCopyArea(dpy, shape, resshape, Context->gc.Shape, 0, 0, width, height, 0, 0);
      object = new WlPixmap(width, height, respix, resshape, hole);
      WL_PAINT(bg)->draw_bitmap(object, 0, 0, width, height, shape);
      WL_PAINT(fg)->draw_bitmap(object, 0, 0, width, height, pixmap);
    } else {
      object = new WlPixmap(width, height);
      WL_PAINT(bg)->draw_rect(object, 0, 0, width, height, 0);
      WL_PAINT(fg)->draw_bitmap(object, 0, 0, width, height, pixmap);
    }
  } else if ((int) depth == Context->depth) {	/* PIXMAP */
    respix = XCreatePixmap(dpy, Context->Root(), width, height, Context->depth);
    XCopyArea(dpy, pixmap, respix, Context->gc.Work, 0, 0, width, height, 0, 0);
    if (shape) {
      resshape = XCreatePixmap(dpy, Context->Root(), width, height, 1);
      XCopyArea(dpy, shape, resshape, Context->gc.Shape, 0, 0, width, height, 0, 0);
    } else 
      resshape = 0;
    object = new WlPixmap(width, height, respix, resshape, hole);
  } else {			/* ERROR */
    gwm_warning("Pixmap is of bad depth, using default instead", 0);
    return make_default_pixmap(fg, bg);
  }
  return pixmap2scm(object);
}

Pixmap MakeDefaultBitmap(Window root)
{
  Pixmap pm;
  if (!(pm = XCreateBitmapFromData(dpy, root,
                                   (char*) def_bitmap_bits,
                                   def_bitmap_width, def_bitmap_height)))
    scm_memory_error(0);
  return pm;
}

struct Graphic_desc {
  int width, height;
  int x_hot, y_hot;
  Pixmap bitmap;
  Paint* color;
};

SCM_DEFINE(make_pixmap, "make-pixmap", 1, 0, 1,
           (SCM arg0, SCM args),
           "(make-pixmap width height [key val])"
           "(make-pixmap filename [key val] ...)"
           "(make-pixmap background [filename1 color1] ...)"
           "Construct a pixmap either with dimensions width and height, or from"
           "the bitmap in filename, or by combining several bitmap files using"
           "different colors. The first form considers the key :background,"
           "and the second form the keys :foreground and :background.")
{
  int i, cn;
  int argc;
  int left = 0, right = 0, up = 0, down = 0;
  int width, height, tmp;
  WlPixmap *object;
  Graphic_desc* graphics;
  Graphic_desc* gr;
  SCM a1, a2, arg, sub, ctx;
  SCM bg, fg;
  
  argc = wl_separate_context(args, sub, ctx, cn, s_make_pixmap);
  args = sub;
  if (argc == 1) {
    must_be_number(s_make_pixmap, arg0, 1);
    must_be_number(s_make_pixmap, SCM_CAR(args), 2);
    bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
    if (!WLPAINTP(bg))
      gwm_wrong_type_arg(s_make_pixmap, 0, bg, "color or pixmap");
    width = gh_scm2int(arg0);
    height = gh_scm2int(SCM_CAR(args));
    if (width <= 0 || height <= 0)
      gwm_misc_error(s_make_pixmap, "Invalid pixmap size, ~A", gh_list(arg0, SCM_CAR(args), SCM_UNDEFINED));
    object = new WlPixmap(width, height);
    WL_PAINT(bg)->draw_rect(object, 0, 0, width, height, 0);
    return pixmap2scm(object);
  }
  if (argc % 2)
    gwm_wrong_num_args(s_make_pixmap, argc+1);
  if (argc == 0) {		/* one arg re-calls with default colors */
    must_be_string(s_make_pixmap, arg0, 1);
    fg = gwm_get_keyword(k_foreground, ctx, cn, Context->pixel.Black);
    bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
    args = gh_list(arg0, fg, SCM_UNDEFINED);
    argc = 2;
  } else
    bg = arg0;
  if (!WLPAINTP(bg))
    gwm_wrong_type_arg(s_make_pixmap, 1, bg, "color or pixmap");
  graphics = new Graphic_desc[argc/2];
  
  /* first determine the size of the final pixmap and load the bitmaps */
  for (gr = graphics, i = 0, arg=args; i < argc; gr++, i+=2) {
    a1=SCM_CAR(arg), arg=SCM_CDR(arg), a2=SCM_CAR(arg), arg=SCM_CDR(arg);
    must_be_string(s_make_pixmap, a1, i+2);
    if (!WLPAINTP(a2))
      gwm_wrong_type_arg(s_make_pixmap, i+3, a2, "color or pixmap");
    gr->bitmap = gwm_raw_bitmap_load(a1, &gr->width, &gr->height,
                                     &gr->x_hot, &gr->y_hot);
    gr->color = WL_PAINT(a2);
    tmp = (gr->x_hot != -1 ? gr->x_hot : gr->width / 2);
    left = Max(left, tmp);
    right = Max(right, gr->width - tmp);
    tmp = (gr->y_hot != -1 ? gr->y_hot : gr->height / 2);
    up = Max(up, tmp);
    down = Max(down, gr->height - tmp);
  }
  
  /* then lay the graphics one on top of another */
  object = new WlPixmap(left+right, up+down);
  WL_PAINT(bg)->draw_rect(object, 0, 0, left+right, up+down, 0);
  for (gr = graphics, i = 0, arg=args; i < argc; gr++, i+=2) {
    gr->color->draw_bitmap(object,
                           left - (gr->x_hot != -1 ? gr->x_hot : gr->width / 2),
                           up - (gr->y_hot != -1 ? gr->y_hot : gr->height / 2),
                           gr->width, gr->height, gr->bitmap);
  }
  
  delete [] graphics;
  return pixmap2scm(object);
}

SCM_DEFINE(make_pixmap_label, "make-label", 1, 0, 1,
           (SCM label, SCM args),
           "Make a pixmap with a text label. Keys are:"
           ":font"
           ":background"
           ":foreground"
           ":horizontal-margin"
           ":vertical-margin"
           ":angle             angle of text, clockwise in degrees"
           ":mirrored          mirror-reverse text if true")
{
  int x, y, width, height;
  int vmarg, hmarg;
  WlPixmap* object;
  SCM font;
  SCM fg, bg, ang, mir;
  SCM sub, ctx;
  int n, cn, rot = 0;
  Pixmap mask;
  char* str;
  
  if ((n = wl_separate_context(args, sub, ctx, cn, s_make_pixmap_label)) != 0)
    gwm_wrong_num_args(s_make_pixmap_label, n+1);
  bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
  if (!WLPAINTP(bg))
    gwm_wrong_type_arg(s_make_pixmap_label, 0, bg, "color or pixmap");
  fg = gwm_get_keyword(k_foreground, ctx, cn, Context->pixel.Black);
  if (!WLPAINTP(fg))
    gwm_wrong_type_arg(s_make_pixmap_label, 0, fg, "color or pixmap");
  font = gwm_get_keyword(k_font, ctx, cn, DefaultFont);
  if (!WLFONTP(font))
    gwm_wrong_type_arg(s_make_pixmap_label, 0, font, "font");
  hmarg = wl_getposint(gwm_get_keyword(k_horizontal_margin, ctx, cn, SCM_BOOL_F), s_make_pixmap_label);
  if (hmarg < 0) hmarg = 0;
  vmarg = wl_getposint(gwm_get_keyword(k_vertical_margin, ctx, cn, SCM_BOOL_F), s_make_pixmap_label);
  if (vmarg < 0) vmarg = 0;
  ang = gwm_get_keyword(k_angle, ctx, cn, gh_double2scm(0.0));
  if (!gh_number_p(ang))
    gwm_wrong_type_arg(s_make_pixmap_label, 0, ang, "real number");
  mir = gwm_get_keyword(k_mirrored, ctx, cn, SCM_BOOL_F);
  str = wl_getstring(label, s_make_pixmap_label, 1);
  if (mir != SCM_BOOL_F || gh_scm2double(ang) != 0.0) {
    rot = 1;
    mask = WL_FONT(font)->rotate_text(str, gh_scm2double(ang),
                                      (mir != SCM_BOOL_F ? 1 : 0),
                                      x, y, width, height);
  } else
    WL_FONT(font)->dimensions(str, x, y, width, height);
  x = hmarg + x;
  y = vmarg + y;
  width += 2 * hmarg;
  height += 2 * vmarg;
  if (width <= 0) width = 1;
  if (height <= 0) height = 1;
  object = new WlPixmap(width, height);
  WL_PAINT(bg)->draw_rect(object, 0, 0, width, height, 0);
  if (rot)
    WL_PAINT(fg)->draw_bitmap(object, hmarg, vmarg,
                              width - 2*hmarg, height - 2*vmarg,
                              mask);
  else
    WL_PAINT(fg)->draw_text(object, x, y, WL_FONT(font), str);
  delete [] str;
  return pixmap2scm(object);
}

SCM_DEFINE(load_xpm_pixmap, "load-pixmap", 1, 0, 1,
           (SCM filename, SCM args),
           "(load-pixmap filename [symbol color] ... [key val] ...)"
           "Loads a pixmap from file, possibly substituting colors according to"
           "the (symbol - color) specifications. Possible keys are:"
           ":xpm-closeness    color closeness value used by XpmReadFileToPixmap"
           ":foreground       (only used if reading of the file goes wrong)"
           ":background       (  - ' ' - )"
           ":shape            'hole or 'transparent, how to treat transparent areas")
{
  SCM name;
  int argc, cn;
  XpmAttributes xpmatt;
  XpmColorSymbol *colorsymbols = 0;
  int ncolorsymbols = 0;
  Pixmap pixmap, pixmapshape;
  int i;
  int res;
  SCM arg, a1, a2, sub, ctx;
  SCM fg, bg, sh;
  int cl;
  char* str;
  argc = wl_separate_context(args, sub, ctx, cn, s_load_xpm_pixmap);
  if (argc % 2)
    gwm_wrong_num_args(s_load_xpm_pixmap, argc+1);
  must_be_string(s_load_xpm_pixmap, filename, 1);
  cl = wl_getint(gwm_get_keyword(k_xpm_closeness, ctx, cn, gh_int2scm(0)), s_load_xpm_pixmap);
  fg = gwm_get_keyword(k_foreground, ctx, cn, Context->pixel.Black);
  if (!WLPAINTP(fg))
    gwm_wrong_type_arg(s_load_xpm_pixmap, 0, fg, "color or pixmap");
  bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
  if (!WLPAINTP(bg))
    gwm_wrong_type_arg(s_load_xpm_pixmap, 0, bg, "color or pixmap");
  sh = gwm_get_keyword(k_shape, ctx, cn, SCM_UNDEFINED);
  if (sh != SCM_UNDEFINED) {
    if (sh != WA_transparent && sh != WA_hole)
      gwm_wrong_type_arg(s_load_xpm_pixmap, 0, sh, "'transparent or 'hole");
  }
  name = scm_search_path(SCM_VARIABLE_REF(v_load_path),
                         filename,
                         scm_listify(scm_makfrom0str(""),
                                     scm_makfrom0str(PIXMAP_EXTENSION),
                                     SCM_UNDEFINED));
  if (!name || name == SCM_BOOL_F) {
    gwm_warning("Cannot find Pixmap ~A, using default instead", filename);
    return make_default_pixmap(fg, bg);
  }
  if (argc) {
    ncolorsymbols = argc/2;
    colorsymbols = new XpmColorSymbol[ncolorsymbols];
    for (i=0, arg=sub; i<ncolorsymbols; i++) {
      a1=SCM_CAR(arg), arg=SCM_CDR(arg), a2=SCM_CAR(arg), arg=SCM_CDR(arg);
      if (!gh_symbol_p(a1) && !gh_string_p(a1))
        gwm_wrong_type_arg(s_load_xpm_pixmap, i*2+2, a1, "symbol or string");
      if (!gh_symbol_p(a2) && !gh_string_p(a2))
        gwm_wrong_type_arg(s_load_xpm_pixmap, i*2+3, a2, "symbol or string");
    }
    for (i=0, arg=sub; i<ncolorsymbols; i++) {
      a1=SCM_CAR(arg), arg=SCM_CDR(arg), a2=SCM_CAR(arg), arg=SCM_CDR(arg);
      colorsymbols[i].name = (gh_symbol_p(a1) ? wl_getsymbol(a1, "") : wl_getstring(a1, ""));
      colorsymbols[i].value = (gh_symbol_p(a2) ? wl_getsymbol(a2, "") : wl_getstring(a2, ""));
      colorsymbols[i].pixel = 0;
    }
  }
  xpmatt.valuemask = XpmVisual|XpmColormap|XpmDepth|XpmColorSymbols;
  xpmatt.visual = DefaultVisual(dpy, Context->ScreenNum());
  xpmatt.colormap = DefaultColormap(dpy, Context->ScreenNum());
  xpmatt.depth = Context->depth;
  xpmatt.colorsymbols = colorsymbols;
  xpmatt.numsymbols = ncolorsymbols;
  if (cl) {
    xpmatt.closeness = cl;
    xpmatt.valuemask |= XpmCloseness;
  }
  str = wl_getstring(name, s_load_xpm_pixmap);
  res = XpmReadFileToPixmap(dpy, Context->Root(), str,
                            &pixmap, &pixmapshape, &xpmatt);
  delete [] str;
  if (colorsymbols) {
    for (i=0; i<ncolorsymbols; i++) {
      delete [] colorsymbols[i].name;
      delete [] colorsymbols[i].value;
    }
    delete [] colorsymbols;
  }
  switch (res) {
  case PixmapSuccess:
    return pixmap2scm(new WlPixmap(xpmatt.width, xpmatt.height, pixmap,
                                   pixmapshape, (sh == WA_hole ? 1 :0)));
  case PixmapOpenFailed:
    gwm_warning("Cannot open file ~A, using default instead", name);
    return make_default_pixmap(fg, bg);
  case PixmapFileInvalid:
    gwm_warning("File ~A is not a XPM pixmap, using default instead", name);
    return make_default_pixmap(fg, bg);
  case PixmapNoMemory:
    scm_memory_error(s_load_xpm_pixmap);
    break;
  default:
    gwm_misc_error(s_load_xpm_pixmap, "Error while reading pixmap ~A", name);
    break;
  }
  return SCM_UNSPECIFIED; // not reached
}

SCM_DEFINE(load_image, "load-image", 1, 0, 1,
           (SCM filename, SCM args),
           "Loads a pixmap from an image file."
           "Only supported if Gwm is compiled with Imlib or Imlib2. Keys are:"
           ":shape            'hole or 'transparent, how to treat transparent areas"
           ":width            scale image to width"
           ":height           scale image to height"
           ":borderwidth      borders exluded from scaling, one width or (left right top bottom)"
           ":rotate           rotation of the result, 'left, 'right, or 'half"
           ":crop             use the given rectangle (x y width height) from the (possibly scaled and rotated) image"
           ":foreground       (only used if reading of the file goes wrong)"
           ":background       (  - ' ' - )")
{
#ifdef HAVE_IMLIB_2
  SCM name;
  int argc, cn, slen;
  Pixmap pixmap, pixmapshape;
  Pixmap respix, resshape;
  int width, height;
  Window root;
  SCM sub, ctx;
  SCM fg, bg, sh, brd, crp, rot;
  Imlib_Image im;
  Imlib_Border border;
  int crp_x, crp_y, crp_w, crp_h, im_width, im_height;
  char* str;
  argc = wl_separate_context(args, sub, ctx, cn, s_load_image);
  if (argc != 0)
    gwm_wrong_num_args(s_load_image, argc+1);
  must_be_string(s_load_image, filename, 1);
  sh = gwm_get_keyword(k_shape, ctx, cn, SCM_UNDEFINED);
  if (sh != SCM_UNDEFINED) {
    if (sh != WA_transparent && sh != WA_hole)
      gwm_wrong_type_arg(s_load_image, 0, sh, "'transparent or 'hole");
  }
  width = wl_getposint(gwm_get_keyword(k_width, ctx, cn, SCM_BOOL_F), s_load_image);
  height = wl_getposint(gwm_get_keyword(k_height, ctx, cn, SCM_BOOL_F), s_load_image);
  brd = gwm_get_keyword(k_borderwidth, ctx, cn, SCM_BOOL_F);
  if (brd != SCM_BOOL_F) {
    if (!scm_is_integer(brd) &&
        (scm_ilength(brd) != 4 ||
         !scm_is_integer(SCM_CAR(brd)) ||
         !scm_is_integer(SCM_CAR(SCM_CDR(brd))) ||
         !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(brd)))) ||
         !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(brd)))))))
      gwm_wrong_type_arg(s_load_image, 0, brd, "integer or list of four integers");
  }
  crp = gwm_get_keyword(k_crop, ctx, cn, SCM_BOOL_F);
  if (crp != SCM_BOOL_F) {
    if (scm_ilength(crp) != 4 ||
        !scm_is_integer(SCM_CAR(crp)) ||
        !scm_is_integer(SCM_CAR(SCM_CDR(crp))) ||
        !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(crp)))) ||
        !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(crp))))))
      gwm_wrong_type_arg(s_load_image, 0, crp, "list of four integers");
  }
  rot = gwm_get_keyword(k_rotate, ctx, cn, SCM_BOOL_F);
  fg = gwm_get_keyword(k_foreground, ctx, cn, Context->pixel.Black);
  if (!WLPAINTP(fg))
    gwm_wrong_type_arg(s_load_image, 0, fg, "color or pixmap");
  bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
  if (!WLPAINTP(bg))
    gwm_wrong_type_arg(s_load_image, 0, bg, "color or pixmap");
  name = scm_search_path(SCM_VARIABLE_REF(v_load_path),
                         filename,
                         scm_listify(scm_makfrom0str(""),
                                     SCM_UNDEFINED));
  if (!name || name == SCM_BOOL_F) {
    gwm_warning("Cannot find image file ~A, using default instead", filename);
    return make_default_pixmap(fg, bg);
  }
  imlib_context_set_display(dpy);
  imlib_context_set_visual(DefaultVisual(dpy, Context->ScreenNum()));
  imlib_context_set_colormap(DefaultColormap(dpy, Context->ScreenNum()));
  str = wl_getstring(name, s_load_image);
  im = imlib_load_image(str);
  delete [] str;
  if (!im) {
    gwm_warning("Failed loading image file ~A, using default instead", name);
    return make_default_pixmap(fg, bg);
  }
  imlib_context_set_image(im);
  im_width = imlib_image_get_width();
  im_height = imlib_image_get_height();
  if (width <= 0) width = im_width;
  if (height <= 0) height = im_height;
  if (brd != SCM_BOOL_F) {
    if (scm_is_integer(brd)) {
      border.left = border.right = border.top = border.bottom = gh_scm2int(brd);
    } else {
      border.left = gh_scm2int(SCM_CAR(brd));
      border.right = gh_scm2int(SCM_CAR(SCM_CDR(brd)));
      border.top = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(brd))));
      border.bottom = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(brd)))));
    }
    imlib_image_set_border(&border);
  }
  if (rot != SCM_BOOL_F) {
    if (gh_symbol_p(rot) || gh_string_p(rot)) {
      int tmp;
      int len = (gh_string_p(rot) ? scm_i_string_length(rot) : scm_i_symbol_length(rot));
      const char* chars = (gh_string_p(rot) ? scm_i_string_chars(rot) : scm_i_symbol_chars(rot));
      if (len == 4 && !strncmp(chars, "left", 4)) {
        imlib_image_orientate(3);
        tmp = im_width, im_width = im_height, im_height = tmp;
      } else if (len == 5 && !strncmp(chars, "right", 5)) {
        imlib_image_orientate(1);
        tmp = im_width, im_width = im_height, im_height = tmp;
      } else if (len == 4 && !strncmp(chars, "half", 4)) {
        imlib_image_orientate(2);
      }
    }
  }
  if (crp != SCM_BOOL_F) {
    crp_x = gh_scm2int(SCM_CAR(crp));
    crp_y = gh_scm2int(SCM_CAR(SCM_CDR(crp)));
    crp_w = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(crp))));
    crp_h = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(crp)))));
  } else
    crp_x = crp_y = 0, crp_w = width, crp_h = height;

  imlib_context_set_drawable(Context->Root());
  imlib_render_pixmaps_for_whole_image_at_size(&pixmap, &pixmapshape, width, height);
  respix = XCreatePixmap(dpy, Context->Root(), crp_w, crp_h, Context->depth);
  XCopyArea(dpy, pixmap, respix, Context->gc.Work, crp_x, crp_y, crp_w, crp_h, 0, 0);
  if (imlib_image_has_alpha()) {
    resshape = XCreatePixmap(dpy, Context->Root(), crp_w, crp_h, 1);
    XCopyArea(dpy, pixmapshape, resshape, Context->gc.Shape, crp_x, crp_y, crp_w, crp_h, 0, 0);
  } else
    resshape = 0;
  imlib_free_pixmap_and_mask(pixmap);
  imlib_free_image();
  return pixmap2scm(new WlPixmap(crp_w, crp_h, respix, resshape, 
                                 (sh == WA_hole ? 1 :0)));
#elif defined HAVE_IMLIB
  SCM name;
  int argc, cn, slen;
  Pixmap pixmap, pixmapshape;
  Pixmap respix, resshape;
  int width, height;
  Window root;
  SCM sub, ctx;
  SCM fg, bg, sh, brd, crp, rot;
  ImlibInitParams params;
  ImlibImage *im;
  ImlibBorder border;
  static ImlibData* imlib_id = 0;
  char* str;
  argc = wl_separate_context(args, sub, ctx, cn, s_load_image);
  if (argc != 0)
    gwm_wrong_num_args(s_load_image, argc+1);
  must_be_string(s_load_image, filename, 1);
  sh = gwm_get_keyword(k_shape, ctx, cn, SCM_UNDEFINED);
  if (sh != SCM_UNDEFINED) {
    if (sh != WA_transparent && sh != WA_hole)
      gwm_wrong_type_arg(s_load_image, 0, sh, "'transparent or 'hole");
  }
  width = wl_getposint(gwm_get_keyword(k_width, ctx, cn, SCM_BOOL_F), s_load_image);
  height = wl_getposint(gwm_get_keyword(k_height, ctx, cn, SCM_BOOL_F), s_load_image);
  brd = gwm_get_keyword(k_borderwidth, ctx, cn, SCM_BOOL_F);
  if (brd != SCM_BOOL_F) {
    if (!scm_is_integer(brd) &&
        (scm_ilength(brd) != 4 ||
         !scm_is_integer(SCM_CAR(brd)) ||
         !scm_is_integer(SCM_CAR(SCM_CDR(brd))) ||
         !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(brd)))) ||
         !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(brd)))))))
      gwm_wrong_type_arg(s_load_image, 0, brd, "integer or list of four integers");
  }
  crp = gwm_get_keyword(k_crop, ctx, cn, SCM_BOOL_F);
  if (crp != SCM_BOOL_F) {
    if (scm_ilength(crp) != 4 ||
        !scm_is_integer(SCM_CAR(crp)) ||
        !scm_is_integer(SCM_CAR(SCM_CDR(crp))) ||
        !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(crp)))) ||
        !scm_is_integer(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(crp))))))
      gwm_wrong_type_arg(s_load_image, 0, crp, "list of four integers");
  }
  rot = gwm_get_keyword(k_rotate, ctx, cn, SCM_BOOL_F);
  fg = gwm_get_keyword(k_foreground, ctx, cn, Context->pixel.Black);
  if (!WLPAINTP(fg))
    gwm_wrong_type_arg(s_load_image, 0, fg, "color or pixmap");
  bg = gwm_get_keyword(k_background, ctx, cn, Context->pixel.White);
  if (!WLPAINTP(bg))
    gwm_wrong_type_arg(s_load_image, 0, bg, "color or pixmap");
  name = scm_search_path(SCM_VARIABLE_REF(v_load_path),
                         filename,
                         scm_listify(scm_makfrom0str(""),
                                     SCM_UNDEFINED));
  if (!name || name == SCM_BOOL_F) {
    gwm_warning("Cannot find image file ~A, using default instead", filename);
    return make_default_pixmap(fg, bg);
  }
  if (imlib_id == 0) {
    params.visualid = DefaultScreen(dpy);
    params.flags = PARAMS_VISUALID;
    imlib_id = Imlib_init_with_params (dpy, &params);
    if (!imlib_id)
      gwm_misc_error(s_load_image, "Failed to initialize Imlib.", 0);
  }
  str = wl_getstring(name, s_load_image);
  im = Imlib_load_image(imlib_id, str);
  delete [] str;
  if (!im) {
    gwm_warning("Failed loading image file ~A, using default instead", name);
    return make_default_pixmap(fg, bg);
  }
  if (width <= 0) width = im->rgb_width;
  if (height <= 0) height = im->rgb_height;
  if (brd != SCM_BOOL_F) {
    if (scm_is_integer(brd)) {
      border.left = border.right = border.top = border.bottom = gh_scm2int(brd);
    } else {
      border.left = gh_scm2int(SCM_CAR(brd));
      border.right = gh_scm2int(SCM_CAR(SCM_CDR(brd)));
      border.top = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(brd))));
      border.bottom = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(brd)))));
    }
    Imlib_set_image_border (imlib_id, im, &border);
  }
  if (rot != SCM_BOOL_F) {
    if (gh_symbol_p(rot) || gh_string_p(rot)) {
      int len = (gh_string_p(rot) ? scm_i_string_length(rot) : scm_i_symbol_length(rot));
      const char* chars = (gh_string_p(rot) ? scm_i_string_chars(rot) : scm_i_symbol_chars(rot));
      if (len == 4 && !strncmp(chars, "left", 4)) {
        Imlib_rotate_image(imlib_id, im, 0);
        Imlib_flip_image_vertical(imlib_id, im);
      } else if (len == 5 && !strncmp(chars, "right", 5)) {
        Imlib_rotate_image(imlib_id, im, 0);
        Imlib_flip_image_horizontal(imlib_id, im);
      } else if (len == 4 && !strncmp(chars, "half", 4)) {
        Imlib_flip_image_horizontal(imlib_id, im);
        Imlib_flip_image_vertical(imlib_id, im);
      }
    }
  }
  Imlib_render(imlib_id, im, width, height);
  pixmap = Imlib_move_image(imlib_id, im);
  pixmapshape = Imlib_move_mask(imlib_id, im);
  if (crp != SCM_BOOL_F) {
    crp_x = gh_scm2int(SCM_CAR(crp));
    crp_y = gh_scm2int(SCM_CAR(SCM_CDR(crp)));
    crp_w = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(crp))));
    crp_h = gh_scm2int(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(crp)))));
  } else {
    crp_x = crp_y = 0, crp_w = width, crp_h = height;
  }
  respix = XCreatePixmap(dpy, Context->Root(), crp_w, crp_h, Context->depth);
  XCopyArea(dpy, pixmap, respix, Context->gc.Work, crp_x, crp_y, crp_w, crp_h, 0, 0);
  if (pixmapshape) {
    resshape = XCreatePixmap(dpy, Context->Root(), crp_w, crp_h, 1);
    XCopyArea(dpy, pixmapshape, resshape, Context->gc.Shape, crp_x, crp_y, crp_w, crp_h, 0, 0);
  } else 
    resshape = 0;
  Imlib_free_pixmap(imlib_id, pixmap);
  Imlib_destroy_image(imlib_id, im);
  return pixmap2scm(new WlPixmap(crp_w, crp_h, respix, resshape, 
                                 (sh == WA_hole ? 1 :0)));
#else
  gwm_warning("Image support was not included when compiling Gwm.", 0);
  return SCM_BOOL_F;
#endif
}


void Paint::shape_border(Window hook, Box* box, int xoff, int yoff, int sub)
{
  XRectangle rects[4];
  if (box->borderwidth) {
    rects[0].x = - box->borderwidth;
    rects[0].y = - box->borderwidth;
    rects[0].width = box->borderwidth;
    rects[0].height = box->height + 2 * box->borderwidth;
    rects[1].x = 0;
    rects[1].y = - box->borderwidth;
    rects[1].width = box->width;
    rects[1].height = box->borderwidth;
    rects[2].x = 0;
    rects[2].y = box->height;
    rects[2].width = box->width;
    rects[2].height = box->borderwidth;
    rects[3].x = box->width;
    rects[3].y = - box->borderwidth;
    rects[3].width = box->borderwidth;
    rects[3].height = box->height + 2 * box->borderwidth;
    XShapeCombineRectangles(dpy, hook, ShapeBounding, xoff, yoff, rects, 4,
                            (sub ? ShapeSubtract : ShapeUnion), 0); 
  }
}

void Paint::shape_background(Window hook, Box* box, int xoff, int yoff, int sub)
{
  XRectangle rect;
  if (box->nonzerosize()) {
    rect.x = 0;
    rect.y = 0;
    rect.width = box->width;
    rect.height = box->height;
    XShapeCombineRectangles(dpy, hook, ShapeBounding, xoff, yoff, &rect, 1, 
                            (sub ? ShapeSubtract : ShapeUnion), 0); 
  }
}

void Paint::shape_border_pixmap(Window hook, Box* box, int xoff, int yoff,
                                Pixmap pm, Pixmap pm2, int pwdt, int phgt)
{
  if (box->borderwidth) {
    shape_rect_pixmap(hook, xoff - box->borderwidth, yoff - box->borderwidth,
                      box->borderwidth, box->height + 2 * box->borderwidth,
                      - box->borderwidth, - box->borderwidth, pm, pm2, pwdt, phgt);
    shape_rect_pixmap(hook, xoff, yoff - box->borderwidth,
                      box->width, box->borderwidth,
                      0, - box->borderwidth, pm, pm2, pwdt, phgt);
    shape_rect_pixmap(hook, xoff, yoff + box->height,
                      box->width, box->borderwidth,
                      0, box->height, pm, pm2, pwdt, phgt);
    shape_rect_pixmap(hook, xoff + box->width, yoff - box->borderwidth,
                      box->borderwidth, box->height + 2 * box->borderwidth,
                      box->width, - box->borderwidth, pm, pm2, pwdt, phgt);
  }
}

void Paint::shape_background_pixmap(Window hook, Box* box, int xoff, int yoff,
                                    Pixmap pm, Pixmap pm2, int pwdt, int phgt)
{
  if (box->nonzerosize())
    shape_rect_pixmap(hook, xoff, yoff, box->width, box->height,
                      0, 0, pm, pm2, pwdt, phgt);
}

void Paint::shape_rect_pixmap(Window hook, int x, int y, int wdt, int hgt, 
                              int xoff, int yoff, Pixmap pm, Pixmap pm2, int pwdt, int phgt)
{
  static Pixmap pixmap = 0;
  static int iwdt = 64;
  static int ihgt = 64;
  XGCValues gcval;
  int i, j, nx, ny, cx, cy, rx, ry;
  if (wdt == pwdt && hgt == phgt && xoff == 0 && yoff == 0) { // simple case
    XShapeCombineMask(dpy, hook, ShapeBounding, x, y, pm, ShapeSubtract);
    if (pm2)
      XShapeCombineMask(dpy, hook, ShapeBounding, x, y, pm2, ShapeSubtract);
  } else { // complex case
    if (pwdt > iwdt || phgt > ihgt) {
      if (pixmap)
        XFreePixmap(dpy, pixmap);
      pixmap = 0;
      iwdt = Max(pwdt, iwdt);
      ihgt = Max(phgt, ihgt);
    }
    if (!pixmap)
      pixmap = XCreatePixmap(dpy, Context->Root(), iwdt, ihgt, 1);
    cx = ((iwdt / pwdt) * pwdt);
    nx = (wdt - iwdt - 1 + cx) / cx;
    rx = wdt - cx * nx;
    if (rx==cx) rx=0, nx++;
    cy = ((ihgt / phgt) * phgt);
    ny = (hgt - ihgt - 1 + cy) / cy;
    ry = hgt -cy * ny;
    if (ry==cy) ry=0, ny++;
    if (!pm2 || (nx && ny) || (!nx && !ny))
      gcval.stipple = pm;
    else
      gcval.stipple = pm2;
    gcval.foreground = 1;
    gcval.ts_x_origin = -xoff;
    gcval.ts_y_origin = -yoff;
    XChangeGC(dpy, Context->gc.ShapeS, 
              GCForeground | GCTileStipXOrigin | GCTileStipYOrigin | GCStipple,
              &gcval);
    XSetForeground(dpy, Context->gc.Shape, 0);
    if (nx) {
      if (ny) {
        XFillRectangle(dpy, pixmap, Context->gc.Shape, 0, 0, iwdt, ihgt);
        XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, cx, cy);
        if (pm2) {
          XSetStipple(dpy, Context->gc.ShapeS, pm2);
          XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, cx, cy);
        }
        for (j=0; j<ny; j++)
          for (i=0; i<nx; i++) {
            XShapeCombineMask(dpy, hook, ShapeBounding,
                              x + i*cx, y + j*cy,
                              pixmap, ShapeSubtract);
          }
      }
      if (ry) {
        XFillRectangle(dpy, pixmap, Context->gc.Shape, 0, 0, iwdt, ihgt);
        if (pm2) {
          XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, cx, ry);
          XSetStipple(dpy, Context->gc.ShapeS, pm);
        }
        XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, cx, ry);
        for (i=0; i<nx; i++) {
          XShapeCombineMask(dpy, hook, ShapeBounding,
                            x + i*cx, y + ny*cy,
                            pixmap, ShapeSubtract);
        }
      }
    }
    if (rx) {
      if (ry) {
        XFillRectangle(dpy, pixmap, Context->gc.Shape, 0, 0, iwdt, ihgt);
        XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, rx, ry);
        if (pm2) {
          XSetStipple(dpy, Context->gc.ShapeS, pm2);
          XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, rx, ry);
        }
        XShapeCombineMask(dpy, hook, ShapeBounding,
                          x + nx*cx, y + ny*cy,
                          pixmap, ShapeSubtract);
      }
      if (ny) {
        XFillRectangle(dpy, pixmap, Context->gc.Shape, 0, 0, iwdt, ihgt);
        if (pm2) {
          XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, rx, cy);
          XSetStipple(dpy, Context->gc.ShapeS, pm);
        }
        XFillRectangle(dpy, pixmap, Context->gc.ShapeS, 0, 0, rx, cy);
        for (j=0; j<ny; j++) {
          XShapeCombineMask(dpy, hook, ShapeBounding,
                            x + nx*cx, y + j*cy,
                            pixmap, ShapeSubtract);
        }
      }
    }
  }
}


void WlColor::paint_border(Window hook, Box* box)
{
  XSetWindowBorder(dpy, hook, pixel);
  if (box->onlyborder()) {
    XSetWindowBackground(dpy, hook, pixel);
    XClearWindow(dpy, hook);
  }
}

void WlColor::paint_background(Window hook, Box* box)
{
  if (!box->onlyborder())
    XSetWindowBackground(dpy, hook, pixel);
  XClearWindow(dpy, hook);
}

void WlColor::paint_border_shape(Window hook, Box* box)
{
  if (box->onlyborder())
    shape_border(hook, box, box->borderwidth, box->borderwidth, 0);
  else
    shape_border(hook, box, 0, 0, 0);
}

void WlColor::paint_background_shape(Window hook, Box* box)
{
  shape_background(hook, box, 0, 0, 0);
}

void WlColor::paint_border_hole(Window hook, Box* box, int xoff, int yoff)
{
  shape_border(hook, box, xoff, yoff, 0);
}

void WlColor::paint_background_hole(Window hook, Box* box, int xoff, int yoff)
{
  shape_background(hook, box, xoff, yoff, 0);
}


void Transparent::paint_border(Window hook, Box* box)
{
  XSetWindowBorder(dpy, hook, WL_PAINT(Context->pixel.Black)->Pixel());
  if (box->onlyborder())
    XSetWindowBackground(dpy, hook, WL_PAINT(Context->pixel.Black)->Pixel());
}

void Transparent::paint_background(Window hook, Box* box)
{
  if (!box->onlyborder())
    XSetWindowBackground(dpy, hook, WL_PAINT(Context->pixel.Black)->Pixel());
}

void Transparent::paint_border_shape(Window hook, Box* box)
{
  if (box->onlyborder())
    shape_border(hook, box, box->borderwidth, box->borderwidth, 1);
  else
    shape_border(hook, box, 0, 0, 1);
}

void Transparent::paint_background_shape(Window hook, Box* box)
{
  shape_background(hook, box, 0, 0, 1);
}

void Transparent::paint_border_hole(Window hook, Box* box, int xoff, int yoff)
{
  if (hole)
    shape_border(hook, box, xoff, yoff, 1);
}

void Transparent::paint_background_hole(Window hook, Box* box, int xoff, int yoff)
{
  if (hole)
    shape_background(hook, box, xoff, yoff, 1);
}


void WlPixmap::paint_border(Window hook, Box* box)
{
  XSetWindowBorderPixmap(dpy, hook, pixmap);
  if (box->onlyborder()) {
    XSetWindowBackgroundPixmap(dpy, hook, pixmap);
    XClearWindow(dpy, hook);
  }
}

void WlPixmap::paint_background(Window hook, Box* box)
{
  if (!box->onlyborder())
    XSetWindowBackgroundPixmap(dpy, hook, pixmap);
  XClearWindow(dpy, hook);
}

void WlPixmap::paint_border_shape(Window hook, Box* box)
{
  if (box->onlyborder()) {
    shape_border(hook, box, box->borderwidth, box->borderwidth, 0);
    if (transmask)
      shape_border_pixmap(hook, box, box->borderwidth, box->borderwidth, transmask, holemask, width, height);
    else if (holemask)
      shape_border_pixmap(hook, box, box->borderwidth, box->borderwidth, holemask, 0, width, height);
  } else {
    shape_border(hook, box, 0, 0, 0);
    if (transmask)
      shape_border_pixmap(hook, box, 0, 0, transmask, holemask, width, height);
    else if (holemask)
      shape_border_pixmap(hook, box, 0, 0, holemask, 0, width, height);
  }
}

void WlPixmap::paint_background_shape(Window hook, Box* box)
{
  shape_background(hook, box, 0, 0, 0);
  if (transmask)
    shape_background_pixmap(hook, box, 0, 0, transmask, holemask, width, height);
  else if (holemask)
    shape_background_pixmap(hook, box, 0, 0, holemask, 0, width, height);
}

void WlPixmap::paint_border_hole(Window hook, Box* box, int xoff, int yoff)
{
  if (holemask)
    shape_border_pixmap(hook, box, xoff, yoff, holemask, 0, width, height);
}

void WlPixmap::paint_background_hole(Window hook, Box* box, int xoff, int yoff)
{
  if (holemask)
    shape_background_pixmap(hook, box, xoff, yoff, holemask, 0, width, height);
}


void WlColor::draw_point(WlPixmap* dest, int x, int y)
{
  internal_draw_line(dest->pixmap, Context->gc.Work, pixel, x, y, x, y);
  internal_draw_line(dest->transmask, Context->gc.Shape, 0, x, y, x, y);
  internal_draw_line(dest->holemask, Context->gc.Shape, 0, x, y, x, y);
  dest->issue_expose_users();
}

void WlColor::draw_line(WlPixmap* dest, int x1, int y1, int x2, int y2)
{
  internal_draw_line(dest->pixmap, Context->gc.Work, pixel, x1, y1, x2, y2);
  internal_draw_line(dest->transmask, Context->gc.Shape, 0, x1, y1, x2, y2);
  internal_draw_line(dest->holemask, Context->gc.Shape, 0, x1, y1, x2, y2);
  dest->issue_expose_users();
}

void WlColor::draw_polygon(WlPixmap* dest, int nump, XPoint* points)
{
  internal_draw_polygon(dest->pixmap, Context->gc.Work, pixel, nump, points);
  internal_draw_polygon(dest->transmask, Context->gc.Shape, 0, nump, points);
  internal_draw_polygon(dest->holemask, Context->gc.Shape, 0, nump, points);
  dest->issue_expose_users();
}

void WlColor::draw_rect(WlPixmap* dest, int x, int y, int w, int h, int bord)
{
  internal_draw_rect(dest->pixmap, Context->gc.Work, pixel, x, y, w, h, bord);
  internal_draw_rect(dest->transmask, Context->gc.Shape, 0, x, y, w, h, bord);
  internal_draw_rect(dest->holemask, Context->gc.Shape, 0, x, y, w, h, bord);
  dest->issue_expose_users();
}

void WlColor::draw_circle(WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  internal_draw_circle(dest->pixmap, Context->gc.Work, pixel, x, y, w, h, bord, ang1, ang2);
  internal_draw_circle(dest->transmask, Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
  internal_draw_circle(dest->holemask, Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
  dest->issue_expose_users();
}

void WlColor::draw_text(WlPixmap* dest, int x, int y, WlFont* font, char* txt)
{
  internal_draw_text(dest->pixmap, Context->gc.Work, pixel, x, y, font->id(), txt);
  internal_draw_text(dest->transmask, Context->gc.Shape, 0, x, y, font->id(), txt);
  internal_draw_text(dest->holemask, Context->gc.Shape, 0, x, y, font->id(), txt);
  dest->issue_expose_users();
}

void WlColor::draw_bitmap(WlPixmap* dest, int x, int y, int w, int h, Pixmap mask)
{
  internal_draw_bitmap(dest->pixmap, Context->gc.Stipple, pixel, x, y, w, h, mask);
  internal_draw_bitmap(dest->transmask, Context->gc.ShapeS, 0, x, y, w, h, mask);
  internal_draw_bitmap(dest->holemask, Context->gc.ShapeS, 0, x, y, w, h, mask);
  dest->issue_expose_users();
}

void Transparent::draw_point(WlPixmap* dest, int x, int y)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_line(dest->pixmap, Context->gc.Work, pixel, x, y, x, y);
  internal_draw_line(dest->transmask, Context->gc.Shape, (hole ? 0 : 1), x, y, x, y);
  internal_draw_line(dest->holemask, Context->gc.Shape, (hole ? 1 : 0), x, y, x, y);
  dest->issue_expose_users();
}

void Transparent::draw_line(WlPixmap* dest, int x1, int y1, int x2, int y2)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_line(dest->pixmap, Context->gc.Work, pixel, x1, y1, x2, y2);
  internal_draw_line(dest->transmask, Context->gc.Shape, (hole ? 0 : 1), x1, y1, x2, y2);
  internal_draw_line(dest->holemask, Context->gc.Shape, (hole ? 1 : 0), x1, y1, x2, y2);
  dest->issue_expose_users();
}

void Transparent::draw_polygon(WlPixmap* dest, int nump, XPoint* points)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_polygon(dest->pixmap, Context->gc.Work, pixel, nump, points);
  internal_draw_polygon(dest->transmask, Context->gc.Shape, (hole ? 0 : 1), nump, points);
  internal_draw_polygon(dest->holemask, Context->gc.Shape, (hole ? 1 : 0), nump, points);
  dest->issue_expose_users();
}

void Transparent::draw_rect(WlPixmap* dest, int x, int y, int w, int h, int bord)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_rect(dest->pixmap, Context->gc.Work, pixel, x, y, w, h, bord);
  internal_draw_rect(dest->transmask, Context->gc.Shape, (hole ? 0 : 1), x, y, w, h, bord);
  internal_draw_rect(dest->holemask, Context->gc.Shape, (hole ? 1 : 0), x, y, w, h, bord);
  dest->issue_expose_users();
}

void Transparent::draw_circle(WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_circle(dest->pixmap, Context->gc.Work, pixel, x, y, w, h, bord, ang1, ang2);
  internal_draw_circle(dest->transmask, Context->gc.Shape, (hole ? 0 : 1), x, y, w, h, bord, ang1, ang2);
  internal_draw_circle(dest->holemask, Context->gc.Shape, (hole ? 1 : 0), x, y, w, h, bord, ang1, ang2);
  dest->issue_expose_users();
}

void Transparent::draw_text(WlPixmap* dest, int x, int y, WlFont* font, char* txt)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_text(dest->pixmap, Context->gc.Work, pixel, x, y, font->id(), txt);
  internal_draw_text(dest->transmask, Context->gc.Shape, (hole ? 0 : 1), x, y, font->id(), txt);
  internal_draw_text(dest->holemask, Context->gc.Shape, (hole ? 1 : 0), x, y, font->id(), txt);
  dest->issue_expose_users();
}

void Transparent::draw_bitmap(WlPixmap* dest, int x, int y, int w, int h, Pixmap mask)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  dest->setup_shape(hole);
  internal_draw_bitmap(dest->pixmap, Context->gc.Stipple, pixel, x, y, w, h, mask);
  internal_draw_bitmap(dest->transmask, Context->gc.ShapeS, (hole ? 0 : 1), x, y, w, h, mask);
  internal_draw_bitmap(dest->holemask, Context->gc.ShapeS, (hole ? 1 : 0), x, y, w, h, mask);
  dest->issue_expose_users();
}

void WlPixmap::setup_shape(int hole)
{
  Pixmap p;
  if (hole ? !holemask : !transmask) {
    p = XCreatePixmap(dpy, Context->Root(), width, height, 1);
    XSetForeground(dpy, Context->gc.Shape, 0);
    XFillRectangle(dpy, p, Context->gc.Shape, 0, 0, width, height);
    if (hole)
      holemask = p;
    else
      transmask = p;
  }
}

void WlPixmap::draw_point(WlPixmap* dest, int x, int y)
{
  internal_draw_line_pm(dest->pixmap, Context->gc.Tile, pixmap, x, y, x, y);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_line_pm(dest->transmask, Context->gc.ShapeT, transmask, x, y, x, y);
  } else
    internal_draw_line(dest->transmask, Context->gc.Shape, 0, x, y, x, y);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_line_pm(dest->holemask, Context->gc.ShapeT, holemask, x, y, x, y);
  } else 
    internal_draw_line(dest->holemask, Context->gc.Shape, 0, x, y, x, y);
  dest->issue_expose_users();
}

void WlPixmap::draw_line(WlPixmap* dest, int x1, int y1, int x2, int y2)
{
  internal_draw_line_pm(dest->pixmap, Context->gc.Tile, pixmap, x1, y1, x2, y2);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_line_pm(dest->transmask, Context->gc.ShapeT, transmask, x1, y1, x2, y2);
  } else
    internal_draw_line(dest->transmask, Context->gc.Shape, 0, x1, y1, x2, y2);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_line_pm(dest->holemask, Context->gc.ShapeT, holemask, x1, y1, x2, y2);
  } else 
    internal_draw_line(dest->holemask, Context->gc.Shape, 0, x1, y1, x2, y2);
  dest->issue_expose_users();
}

void WlPixmap::draw_polygon(WlPixmap* dest, int nump, XPoint* points)
{
  internal_draw_polygon_pm(dest->pixmap, Context->gc.Tile, pixmap, nump, points);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_polygon_pm(dest->transmask, Context->gc.ShapeT, transmask, nump, points);
  } else
    internal_draw_polygon(dest->transmask, Context->gc.Shape, 0, nump, points);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_polygon_pm(dest->holemask, Context->gc.ShapeT, holemask, nump, points);
  } else 
    internal_draw_polygon(dest->holemask, Context->gc.Shape, 0, nump, points);
  dest->issue_expose_users();
}

void WlPixmap::draw_rect(WlPixmap* dest, int x, int y, int w, int h, int bord)
{
  internal_draw_rect_pm(dest->pixmap, Context->gc.Tile, pixmap, x, y, w, h, bord);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_rect_pm(dest->transmask, Context->gc.ShapeT, transmask, x, y, w, h, bord);
  } else
    internal_draw_rect(dest->transmask, Context->gc.Shape, 0, x, y, w, h, bord);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_rect_pm(dest->holemask, Context->gc.ShapeT, holemask, x, y, w, h, bord);
  } else 
    internal_draw_rect(dest->holemask, Context->gc.Shape, 0, x, y, w, h, bord);
  dest->issue_expose_users();
}

void WlPixmap::draw_circle(WlPixmap* dest, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  internal_draw_circle_pm(dest->pixmap, Context->gc.Tile, pixmap, x, y, w, h, bord, ang1, ang2);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_circle_pm(dest->transmask, Context->gc.ShapeT, transmask, x, y, w, h, bord, ang1, ang2);
  } else
    internal_draw_circle(dest->transmask, Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_circle_pm(dest->holemask, Context->gc.ShapeT, holemask, x, y, w, h, bord, ang1, ang2);
  } else 
    internal_draw_circle(dest->holemask, Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
  dest->issue_expose_users();
}

void WlPixmap::draw_text(WlPixmap* dest, int x, int y, WlFont* font, char* txt)
{
  internal_draw_text_pm(dest->pixmap, Context->gc.Tile, pixmap, x, y, font->id(), txt);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_text_pm(dest->transmask, Context->gc.ShapeT, transmask, x, y, font->id(), txt);
  } else
    internal_draw_text(dest->transmask, Context->gc.Shape, 0, x, y, font->id(), txt);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_text_pm(dest->holemask, Context->gc.ShapeT, holemask, x, y, font->id(), txt);
  } else 
    internal_draw_text(dest->holemask, Context->gc.Shape, 0, x, y, font->id(), txt);
  dest->issue_expose_users();
}

void WlPixmap::draw_bitmap(WlPixmap* dest, int x, int y, int w, int h, Pixmap mask)
{
  internal_draw_bitmap_pm(dest->pixmap, Context->gc.Tile, pixmap, x, y, w, h, mask);
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_bitmap_pm(dest->transmask, Context->gc.ShapeT, transmask, x, y, w, h, mask);
  } else
    internal_draw_bitmap(dest->transmask, Context->gc.ShapeS, 0, x, y, w, h, mask);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_bitmap_pm(dest->holemask, Context->gc.ShapeT, holemask, x, y, w, h, mask);
  } else 
    internal_draw_bitmap(dest->holemask, Context->gc.ShapeS, 0, x, y, w, h, mask);
  dest->issue_expose_users();
}

void WlColor::draw_point(Window win, int x, int y)
{
  internal_draw_line(win, Context->gc.Work, pixel, x, y, x, y);
}

void WlColor::draw_point_sh(WlActive* dest, int x, int y)
{
  internal_draw_line(dest->get_transmask(), Context->gc.Shape, 0, x, y, x, y);
  internal_draw_line(dest->get_holemask(), Context->gc.Shape, 0, x, y, x, y);
}

void WlColor::draw_line(Window win, int x1, int y1, int x2, int y2)
{
  internal_draw_line(win, Context->gc.Work, pixel, x1, y1, x2, y2);
}

void WlColor::draw_line_sh(WlActive* dest, int x1, int y1, int x2, int y2)
{
  internal_draw_line(dest->get_transmask(), Context->gc.Shape, 0, x1, y1, x2, y2);
  internal_draw_line(dest->get_holemask(), Context->gc.Shape, 0, x1, y1, x2, y2);
}

void WlColor::draw_polygon(Window win, int nump, XPoint* points)
{
  internal_draw_polygon(win, Context->gc.Work, pixel, nump, points);
}

void WlColor::draw_polygon_sh(WlActive* dest, int nump, XPoint* points)
{
  internal_draw_polygon(dest->get_transmask(), Context->gc.Shape, 0, nump, points);
  internal_draw_polygon(dest->get_holemask(), Context->gc.Shape, 0, nump, points);
}

void WlColor::draw_rect(Window win, int x, int y, int w, int h, int bord)
{
  internal_draw_rect(win, Context->gc.Work, pixel, x, y, w, h, bord);
}

void WlColor::draw_rect_sh(WlActive* dest, int x, int y, int w, int h, int bord)
{
  internal_draw_rect(dest->get_transmask(), Context->gc.Shape, 0, x, y, w, h, bord);
  internal_draw_rect(dest->get_holemask(), Context->gc.Shape, 0, x, y, w, h, bord);
}

void WlColor::draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  internal_draw_circle(win, Context->gc.Work, pixel, x, y, w, h, bord, ang1, ang2);
}

void WlColor::draw_circle_sh(WlActive* dest, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  internal_draw_circle(dest->get_transmask(), Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
  internal_draw_circle(dest->get_holemask(), Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
}

void WlColor::draw_text(Window win, int x, int y, WlFont* font, char* txt)
{
  internal_draw_text(win, Context->gc.Work, pixel, x, y, font->id(), txt);
}

void WlColor::draw_text_sh(WlActive* dest, int x, int y, WlFont* font, char* txt)
{
  internal_draw_text(dest->get_transmask(), Context->gc.Shape, 0, x, y, font->id(), txt);
  internal_draw_text(dest->get_holemask(), Context->gc.Shape, 0, x, y, font->id(), txt);
}

void WlColor::draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask)
{
  internal_draw_bitmap(win, Context->gc.Stipple, pixel, x, y, w, h, mask);
}

void WlColor::draw_bitmap_sh(WlActive* dest, int x, int y, int w, int h, Pixmap mask)
{
  internal_draw_bitmap(dest->get_transmask(), Context->gc.ShapeS, 0, x, y, w, h, mask);
  internal_draw_bitmap(dest->get_holemask(), Context->gc.ShapeS, 0, x, y, w, h, mask);
}

void Transparent::draw_point(Window win, int x, int y)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_line(win, Context->gc.Work, pixel, x, y, x, y);
}

void Transparent::draw_point_sh(WlActive* dest, int x, int y)
{
  dest->setup_shape(hole);
  internal_draw_line(dest->get_transmask(), Context->gc.Shape, (hole ? 0 : 1), x, y, x, y);
  internal_draw_line(dest->get_holemask(), Context->gc.Shape, (hole ? 1 : 0), x, y, x, y);
}

void Transparent::draw_line(Window win, int x1, int y1, int x2, int y2)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_line(win, Context->gc.Work, pixel, x1, y1, x2, y2);
}

void Transparent::draw_line_sh(WlActive* dest, int x1, int y1, int x2, int y2)
{
  dest->setup_shape(hole);
  internal_draw_line(dest->get_transmask(), Context->gc.Shape, (hole ? 0 : 1), x1, y1, x2, y2);
  internal_draw_line(dest->get_holemask(), Context->gc.Shape, (hole ? 1 : 0), x1, y1, x2, y2);
}

void Transparent::draw_polygon(Window win, int nump, XPoint* points)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_polygon(win, Context->gc.Work, pixel, nump, points);
}

void Transparent::draw_polygon_sh(WlActive* dest, int nump, XPoint* points)
{
  dest->setup_shape(hole);
  internal_draw_polygon(dest->get_transmask(), Context->gc.Shape, (hole ? 0 : 1), nump, points);
  internal_draw_polygon(dest->get_holemask(), Context->gc.Shape, (hole ? 1 : 0), nump, points);
}

void Transparent::draw_rect(Window win, int x, int y, int w, int h, int bord)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_rect(win, Context->gc.Work, pixel, x, y, w, h, bord);
}

void Transparent::draw_rect_sh(WlActive* dest, int x, int y, int w, int h, int bord)
{
  dest->setup_shape(hole);
  internal_draw_rect(dest->get_transmask(), Context->gc.Shape, (hole ? 0 : 1), x, y, w, h, bord);
  internal_draw_rect(dest->get_holemask(), Context->gc.Shape, (hole ? 1 : 0), x, y, w, h, bord);
}

void Transparent::draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_circle(win, Context->gc.Work, pixel, x, y, w, h, bord, ang1, ang2);
}

void Transparent::draw_circle_sh(WlActive* dest, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  dest->setup_shape(hole);
  internal_draw_circle(dest->get_transmask(), Context->gc.Shape, (hole ? 0 : 1), x, y, w, h, bord, ang1, ang2);
  internal_draw_circle(dest->get_holemask(), Context->gc.Shape, (hole ? 1 : 0), x, y, w, h, bord, ang1, ang2);
}

void Transparent::draw_text(Window win, int x, int y, WlFont* font, char* txt)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_text(win, Context->gc.Work, pixel, x, y, font->id(), txt);
}

void Transparent::draw_text_sh(WlActive* dest, int x, int y, WlFont* font, char* txt)
{
  dest->setup_shape(hole);
  internal_draw_text(dest->get_transmask(), Context->gc.Shape, (hole ? 0 : 1), x, y, font->id(), txt);
  internal_draw_text(dest->get_holemask(), Context->gc.Shape, (hole ? 1 : 0), x, y, font->id(), txt);
}

void Transparent::draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask)
{
  unsigned long pixel = WL_PAINT(Context->pixel.Black)->Pixel();
  internal_draw_bitmap(win, Context->gc.Stipple, pixel, x, y, w, h, mask);
}

void Transparent::draw_bitmap_sh(WlActive* dest, int x, int y, int w, int h, Pixmap mask)
{
  dest->setup_shape(hole);
  internal_draw_bitmap(dest->get_transmask(), Context->gc.ShapeS, (hole ? 0 : 1), x, y, w, h, mask);
  internal_draw_bitmap(dest->get_holemask(), Context->gc.ShapeS, (hole ? 1 : 0), x, y, w, h, mask);
}

void WlPixmap::draw_point(Window win, int x, int y)
{
  internal_draw_line_pm(win, Context->gc.Tile, pixmap, x, y, x, y);
}

void WlPixmap::draw_point_sh(WlActive* dest, int x, int y)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_line_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, x, y, x, y);
  } else
    internal_draw_line(dest->get_transmask(), Context->gc.Shape, 0, x, y, x, y);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_line_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, x, y, x, y);
  } else 
    internal_draw_line(dest->get_holemask(), Context->gc.Shape, 0, x, y, x, y);
}

void WlPixmap::draw_line(Window win, int x1, int y1, int x2, int y2)
{
  internal_draw_line_pm(win, Context->gc.Tile, pixmap, x1, y1, x2, y2);
}

void WlPixmap::draw_line_sh(WlActive* dest, int x1, int y1, int x2, int y2)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_line_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, x1, y1, x2, y2);
  } else
    internal_draw_line(dest->get_transmask(), Context->gc.Shape, 0, x1, y1, x2, y2);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_line_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, x1, y1, x2, y2);
  } else 
    internal_draw_line(dest->get_holemask(), Context->gc.Shape, 0, x1, y1, x2, y2);
}

void WlPixmap::draw_polygon(Window win, int nump, XPoint* points)
{
  internal_draw_polygon_pm(win, Context->gc.Tile, pixmap, nump, points);
}

void WlPixmap::draw_polygon_sh(WlActive* dest, int nump, XPoint* points)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_polygon_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, nump, points);
  } else
    internal_draw_polygon(dest->get_transmask(), Context->gc.Shape, 0, nump, points);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_polygon_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, nump, points);
  } else 
    internal_draw_polygon(dest->get_holemask(), Context->gc.Shape, 0, nump, points);
}

void WlPixmap::draw_rect(Window win, int x, int y, int w, int h, int bord)
{
  internal_draw_rect_pm(win, Context->gc.Tile, pixmap, x, y, w, h, bord);
}

void WlPixmap::draw_rect_sh(WlActive* dest, int x, int y, int w, int h, int bord)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_rect_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, x, y, w, h, bord);
  } else
    internal_draw_rect(dest->get_transmask(), Context->gc.Shape, 0, x, y, w, h, bord);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_rect_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, x, y, w, h, bord);
  } else 
    internal_draw_rect(dest->get_holemask(), Context->gc.Shape, 0, x, y, w, h, bord);
}

void WlPixmap::draw_circle(Window win, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  internal_draw_circle_pm(win, Context->gc.Tile, pixmap, x, y, w, h, bord, ang1, ang2);
}

void WlPixmap::draw_circle_sh(WlActive* dest, int x, int y, int w, int h, int bord, int ang1, int ang2)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_circle_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, x, y, w, h, bord, ang1, ang2);
  } else
    internal_draw_circle(dest->get_transmask(), Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_circle_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, x, y, w, h, bord, ang1, ang2);
  } else 
    internal_draw_circle(dest->get_holemask(), Context->gc.Shape, 0, x, y, w, h, bord, ang1, ang2);
}

void WlPixmap::draw_text(Window win, int x, int y, WlFont* font, char* txt)
{
  internal_draw_text_pm(win, Context->gc.Tile, pixmap, x, y, font->id(), txt);
}

void WlPixmap::draw_text_sh(WlActive* dest, int x, int y, WlFont* font, char* txt)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_text_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, x, y, font->id(), txt);
  } else
    internal_draw_text(dest->get_transmask(), Context->gc.Shape, 0, x, y, font->id(), txt);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_text_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, x, y, font->id(), txt);
  } else 
    internal_draw_text(dest->get_holemask(), Context->gc.Shape, 0, x, y, font->id(), txt);
}

void WlPixmap::draw_bitmap(Window win, int x, int y, int w, int h, Pixmap mask)
{
  internal_draw_bitmap_pm(win, Context->gc.Tile, pixmap, x, y, w, h, mask);
}

void WlPixmap::draw_bitmap_sh(WlActive* dest, int x, int y, int w, int h, Pixmap mask)
{
  if (transmask) {
    dest->setup_shape(0);
    internal_draw_bitmap_pm(dest->get_transmask(), Context->gc.ShapeT, transmask, x, y, w, h, mask);
  } else
    internal_draw_bitmap(dest->get_transmask(), Context->gc.ShapeS, 0, x, y, w, h, mask);
  if (holemask) {
    dest->setup_shape(1);
    internal_draw_bitmap_pm(dest->get_holemask(), Context->gc.ShapeT, holemask, x, y, w, h, mask);
  } else 
    internal_draw_bitmap(dest->get_holemask(), Context->gc.ShapeS, 0, x, y, w, h, mask);
}


void init_scm_paint()
{
  scm_tc16_wlcolor = scm_make_smob_type("color", 0);
  scm_set_smob_mark(scm_tc16_wlcolor, mark_paint);
  scm_set_smob_free(scm_tc16_wlcolor, free_paint);
  scm_set_smob_print(scm_tc16_wlcolor, print_paint);
  scm_tc16_wlpixmap = scm_make_smob_type("pixmap", 0);
  scm_set_smob_mark(scm_tc16_wlpixmap, mark_paint);
  scm_set_smob_free(scm_tc16_wlpixmap, free_paint);
  scm_set_smob_print(scm_tc16_wlpixmap, print_paint);
#include "paint.x"
}

