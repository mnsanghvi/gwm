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

extern long scm_tc16_wlactive;

class WlActive {
public:
  WlActive(SCM str, SCM ctx, int cn);
  ~WlActive();
  void dimensions(struct Box* box);
  void intrinsic_dimensions(struct Box* box);
  void draw(class Decoration* wob);
  void setup_shape(int hole);
  void register_use(class Decoration* deco);
  void unregister_use(class Decoration* deco);
  void issue_expose_users();
  void mark_dirty();
  void mark_dirty_size();
  void mark_dirty_one(class Decoration* deco);
  SCM get_background() { return background; };
  SCM get_function() { return drawfunc; };
  void set_background(SCM paint) { if (receptive) return; background = paint; mark_dirty(); };
  void set_function(SCM func) { if (receptive) return; drawfunc = func; mark_dirty(); };
  void set_size(int w, int h) { if (receptive) return; width = w; height = h; mark_dirty_size(); };
  void paint_background_shape(Window hook, class Decoration* deco);
  void paint_background_hole(Window hook, class Decoration* deco, int xoff, int yoff);
  void add_point(int x, int y, SCM p);
  void add_line(int x1, int y1, int x2, int y2, SCM p);
  void add_polygon(int np, XPoint* pts, SCM p);
  void add_rect(int x, int y, int w, int h, int b, SCM p);
  void add_circle(int x, int y, int w, int h, int b, int a1, int a2, SCM p);
  void add_text(int x, int y, SCM f, char* t, SCM p);
  void add_bitmap(int x, int y, int w, int h, Pixmap m, SCM p);
  Pixmap& get_transmask();
  Pixmap& get_holemask();
  SCM drawfunc;
  SCM background;
  int width;
  int height;
  Pixmap transmask;
  Pixmap holemask;
  short receptive;
  short draw_directly;
  short dirty;
  class Drawing* drawing;
  class DecoListA* usedby;
  class DecoListA* curr;
  SCM self;
};

SCM make_active(SCM arg1, SCM arg2);

#define WLACTIVEP(x) (SCM_NIMP(x) && SCM_CAR(x) == (SCM)scm_tc16_wlactive)
#define WL_ACTIVE(x) ((WlActive*) SCM_CDR(x))


















