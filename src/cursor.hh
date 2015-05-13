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


