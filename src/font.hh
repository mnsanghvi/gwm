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












