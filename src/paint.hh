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
