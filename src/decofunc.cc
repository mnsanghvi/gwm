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


/*************************************************\
* 						  *
* 	Functions for decoration objects.         *
* 						  *
\*************************************************/

/*  include  */

#include <guile/gh.h>
#include <stdio.h>

#include "gwm.hh"
#include "gwmfunc.hh"
#include "error.hh"
#include "cursor.hh"
#include "active.hh"
#include "paint.hh"
#include "deco.hh"
#include "fsm.hh"
#include "event.hh"
#include "screen.hh"
#include "client.hh"
#include "drawing.hh"
#include "wops.hh"

SCM_DEFINE(wl_deco_p, "deco?", 1, 0, 0,
           (SCM obj),
           "Return true if obj is a decoration.")
{
  return (WLDECOP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM_DEFINE(make_deco, "make-deco", 0, 0, 1,
           (SCM args),
           "([part1] [part2] ... [key val] ...)"
           "Make a decoration object, consisting of the given parts (which can"
           "be decorations, pixmaps, active pixmaps, or empty lists designating"
           "extendable space). Keys are:"
           ":direction       'horizontal, 'vertical or 'center"
           ":width           fixed width"
           ":height          fixed height"
           ":min-width       minimum width of deco"
           ":max-width       maximum width of deco"
           ":min-height      minimum height of deco"
           ":max-height      maximum heigth of deco"
           ":separator       space between parts"
           ":margin          space before first and after last part"
           ":borderwidth     border width"
           ":bordercolor     border color"
           ":background      background color"
           ":behavior        behavior of deco"
           ":cursor          cursor to use over deco"
           ":anchor          specifies fixed point/points for floating parts"
           ":property        association list of properties")
{
    int n, cn;
    SCM ctx, sub;
    Decoration* deco;

    n = wl_separate_context(args, sub, ctx, cn, s_make_deco);
    deco = new Bar(sub, ctx, cn, s_make_deco);
    return deco->scm();
}

SCM_DEFINE(inspect_deco, "inspect-deco", 1, 0, 1,
           (SCM deco, SCM args),
           "Retreive from decoration obj the values of the given keys as a"
           "keyword list. The possible keys are the same as for 'make-deco'.")
{
    must_be_deco(s_inspect_deco, deco, 1);
    return WL_DECO(deco)->inspect(args);
}

SCM_DEFINE(modify_deco, "modify-deco", 1, 0, 1,
           (SCM deco, SCM args),
           "Change the values in deco of the given keys. Possible keys"
           "are the same as for 'make-deco'.")
{
    int n, cn;
    SCM ctx, sub;
    n = wl_separate_context(args, sub, ctx, cn, s_modify_deco);
    if (n != 0)
      gwm_wrong_num_args(s_modify_deco, n+1);
    must_be_deco(s_modify_deco, deco, 1);
    WL_DECO(deco)->modify(ctx, cn, s_modify_deco);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_is_valid, "deco-valid?", 1, 0, 0,
           (SCM deco),
           "Check if deco is opened and valid.")
{
    must_be_deco(s_deco_is_valid, deco, 1);
    if (WL_DECO(deco)->Valid() > 0)
	return SCM_BOOL_T;
    else
	return SCM_BOOL_F;
}

SCM_DEFINE(window_is_valid, "window-valid?", 1, 0, 0,
           (SCM deco),
           "Check if window is an opened and valid window.")
{
    must_be_deco(s_window_is_valid, deco, 1);
    if (WL_DECO(deco)->Valid() > 0 && WL_DECO(deco)->Win())
      return SCM_BOOL_T;
    else
      return SCM_BOOL_F;
}

SCM_DEFINE(deco_width, "deco-width", 1, 0, 0,
           (SCM deco),
           "Get width of deco, excluding border.")
{
    must_be_deco(s_deco_width, deco, 1);
    return gh_int2scm(WL_DECO(deco)->Width());
}

SCM_DEFINE(deco_height, "deco-height", 1, 0, 0,
           (SCM deco),
           "Get height of deco, excluding border.")
{
    must_be_deco(s_deco_height, deco, 1);
    return gh_int2scm(WL_DECO(deco)->Height());
}

SCM_DEFINE(deco_direction, "deco-direction", 1, 0, 0,
           (SCM deco),
           "Get current direction of deco, 'horizontal, 'vertical or 'center.")
{
    must_be_deco(s_deco_direction, deco, 1);
    return WL_DECO(deco)->Direction();
}

SCM_DEFINE(deco_bordercolor, "deco-bordercolor", 1, 0, 0,
           (SCM deco),
           "Get border color of deco.")
{
  must_be_deco(s_deco_bordercolor, deco, 1);
  return WL_DECO(deco)->get_bordercolor();
}

SCM_DEFINE(deco_bordercolor_set, "set-deco-bordercolor!", 2, 0, 0,
           (SCM deco, SCM color),
           "Set border color of deco.")
{
  must_be_deco(s_deco_bordercolor_set, deco, 1);
  if (!WLPAINTP(color))
    gwm_wrong_type_arg(s_deco_bordercolor_set, 2, color, "color or pixmap");
  WL_DECO(deco)->set_bordercolor(color);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_background, "deco-background", 1, 0, 0,
           (SCM deco),
           "Get background color of deco.")
{
  must_be_deco(s_deco_background, deco, 1);
  return WL_DECO(deco)->get_background();
}

SCM_DEFINE(deco_background_set, "set-deco-background!", 2, 0, 0,
           (SCM deco, SCM color),
           "Set background color of deco.")
{
  must_be_deco(s_deco_background_set, deco, 1);
  if (!WLPAINTP(color))
    gwm_wrong_type_arg(s_deco_background_set, 2, color, "color or pixmap");
  WL_DECO(deco)->set_background(color);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_borderwidth, "deco-borderwidth", 1, 0, 0,
           (SCM deco),
           "Get border width of deco.")
{
  must_be_deco(s_deco_borderwidth, deco, 1);
  return gh_int2scm(WL_DECO(deco)->Borderwidth());
}

SCM_DEFINE(deco_borderwidth_set, "set-deco-borderwidth!", 2, 0, 0,
           (SCM deco, SCM width),
           "Set border width of deco.")
{
  must_be_deco(s_deco_borderwidth_set, deco, 1);
  if (width != SCM_BOOL_F && (!scm_is_integer(width) || scm_to_int(width) < 0))
    gwm_wrong_type_arg(s_deco_borderwidth_set, 2, width, "positive number");
  WL_DECO(deco)->set_borderwidth(width);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_property, "deco-properties", 1, 0, 0,
           (SCM deco),
           "Get the property list of deco.")
{
  must_be_deco(s_deco_property, deco, 1);
  return WL_DECO(deco)->get_property();
}
	
SCM_DEFINE(deco_property_set, "set-deco-properties!", 2, 0, 0,
           (SCM deco, SCM property),
           "Set the property list of deco.")
{
  must_be_deco(s_deco_property_set, deco, 1);
  if (property != SCM_EOL && !gh_list_p(property))
    gwm_wrong_type_arg(s_deco_property_set, 2, property, "list");
  WL_DECO(deco)->set_property(property);
  return SCM_UNSPECIFIED;
}
	
SCM_DEFINE(deco_property_get, "get-property", 2, 0, 0,
           (SCM deco, SCM sym),
           "Get property sym of deco.")
{
  SCM prop;
  must_be_deco(s_deco_property_get, deco, 1);
  prop = WL_DECO(deco)->get_property();
  return scm_assq_ref(prop, sym);
}
	
SCM_DEFINE(deco_property_put, "set-property!", 3, 0, 0,
           (SCM deco, SCM sym, SCM val),
           "Set property sym of deco to val.")
{
  SCM prop;
  must_be_deco(s_deco_property_put, deco, 1);
  prop = WL_DECO(deco)->get_property();
  prop = scm_assq_set_x(prop, sym, val);
  WL_DECO(deco)->set_property(prop);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_behavior, "deco-behavior", 1, 0, 0,
           (SCM deco),
           "Get the behavior of deco.")
{
  must_be_deco(s_deco_behavior, deco, 1);
  return WL_DECO(deco)->get_behavior();
}

SCM_DEFINE(deco_behavior_set, "set-deco-behavior!", 2, 0, 0,
           (SCM deco, SCM beh),
           "Set the behavior of deco.")
{
  must_be_deco(s_deco_behavior_set, deco, 1);
  if (beh != SCM_BOOL_F && !WLBEHAVIORP(beh))
    gwm_wrong_type_arg(s_deco_behavior_set, 2, beh, "behavior");
  WL_DECO(deco)->set_behavior(beh);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_cursor, "deco-cursor", 1, 0, 0,
           (SCM deco),
           "Get the cursor used over deco.")
{
  must_be_deco(s_deco_cursor, deco, 1);
  return WL_DECO(deco)->get_cursor();
}

SCM_DEFINE(deco_cursor_set, "set-deco-cursor!", 2, 0, 0,
           (SCM deco, SCM cursor),
           "Set the cursor to use over deco.")
{
  must_be_deco(s_deco_cursor_set, deco, 1);
  if (cursor != SCM_BOOL_F && !WLCURSORP(cursor))
    gwm_wrong_type_arg(s_deco_cursor_set, 2, cursor, "cursor");
  WL_DECO(deco)->set_cursor(cursor);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_anchor, "deco-anchor", 1, 0, 0,
           (SCM deco),
           "Get the anchor of the deco. If a decoration has an anchor, it is"
           "considered to be free-floating, i.e. instead of being placed next to"
           "the other subdecorations, it is placed wherever the anchor specifies.")
{
  must_be_deco(s_deco_anchor, deco, 1);
  return WL_DECO(deco)->get_anchor();
}

SCM_DEFINE(deco_anchor_set, "set-deco-anchor!", 2, 0, 0,
           (SCM deco, SCM anchor),
           "Set the anchor of the deco. If a decoration has an anchor, it is"
           "considered to be free-floating, i.e. instead of being placed next to"
           "the other subdecorations, it is placed wherever the anchor specifies."
           "The anchor can be a list of zero, two or four elements. Zero elements"
           "means no anchor, two fixes the position, and four fixes the position"
           "and size of the decoration in its parent. Each element is either a"
           "positive integer (offset of left/upper edge from left/upper side of"
           "parent), a negative integer (offset minus one of right/lower edge from"
           "right/lower side of parent), or a list of three numbers where the first"
           "is a real number (typically but not necessarily between 0 and 1) as a"
           "proportion in the decoration, the second an integer offset and the"
           "third a real number as a proportion of the parent decoration. For"
           "example, (0.5 0 0.5) means that the centers of the decorations should"
           "be aligned, (0 5 0) means the same as 5, and (1 -3 1) means the same"
           "as -4.")
{
  must_be_deco(s_deco_anchor_set, deco, 1);
  if (anchor != SCM_BOOL_F && !gh_list_p(anchor))
    gwm_wrong_type_arg(s_deco_anchor_set, 2, anchor, "anchor list");
  WL_DECO(deco)->set_anchor(anchor);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_parts, "deco-parts", 1, 0, 0,
           (SCM deco),
           "Get a list of all the sub-decorations of deco")
{
  must_be_deco(s_deco_parts, deco, 1);
  return WL_DECO(deco)->get_parts();
}

SCM_DEFINE(deco_num_parts, "deco-num-parts", 1, 0, 0,
           (SCM deco),
           "Get the number of sub-decorations of deco")
{
  int m;
  must_be_deco(s_deco_num_parts, deco, 1);
  m = WL_DECO(deco)->get_num_parts();
  if (m == -1)
    return SCM_BOOL_F;
  else
    return gh_int2scm(m);
}

SCM_DEFINE(deco_part, "deco-part", 2, 0, 0,
           (SCM deco, SCM num),
           "Get sub-decoration number num of deco.")
{
  must_be_deco(s_deco_parts, deco, 1);
  must_be_number(s_deco_parts, num, 2);
  return WL_DECO(deco)->get_part(gh_scm2int(num));
}

SCM_DEFINE(deco_part_set, "set-deco-part!", 3, 0, 0,
           (SCM deco, SCM num, SCM obj),
           "Replace sub-decoration number num of deco with obj.")
{
  int n, m;
  must_be_deco(s_deco_part_set, deco, 1);
  must_be_number(s_deco_part_set, num, 2);
  if (obj != SCM_EOL && !WLPIXMAPP(obj) && !WLACTIVEP(obj) && !WLDECOP(obj))
    gwm_wrong_type_arg(s_deco_part_set, 3, obj, "deco, pixmap, active pixmap, or '()");
  m = WL_DECO(deco)->get_num_parts();
  n = gh_scm2int(num);
  if (m == -1)
    gwm_misc_error(s_deco_part_set, "Deco ~A can have no parts", deco);
  if (n<1 || n>m)
    scm_out_of_range_pos(s_deco_part_set, num, gh_int2scm(2));
  if (WLDECOP(obj))
    WL_DECO(obj)->assure_free_deco(s_deco_part_set, 3);
  WL_DECO(deco)->set_part(obj, n);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_add_part, "deco-add-part!", 3, 0, 0,
           (SCM deco, SCM num, SCM obj),
           "Add obj as a new subdecoration at position num in deco")
{
  int n, m;
  must_be_deco(s_deco_add_part, deco, 1);
  must_be_number(s_deco_add_part, num, 2);
  if (obj != SCM_EOL && !WLPIXMAPP(obj) && !WLACTIVEP(obj) && !WLDECOP(obj))
    gwm_wrong_type_arg(s_deco_add_part, 3, obj, "deco, pixmap, active pixmap, or '()");
  m = WL_DECO(deco)->get_num_parts();
  n = gh_scm2int(num);
  if (m == -1)
    gwm_misc_error(s_deco_add_part, "Deco ~A can have no parts", deco);
  if (n<1 || n>m+1)
    scm_out_of_range_pos(s_deco_add_part, num, gh_int2scm(2));
  if (WLDECOP(obj))
    WL_DECO(obj)->assure_free_deco(s_deco_add_part, 3);
  WL_DECO(deco)->insert_part(obj, n);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_remove_part, "deco-remove-part!", 2, 0, 0,
           (SCM deco, SCM num),
           "Remove sub-decoration number num of deco.")
{
  int n, m;
  must_be_deco(s_deco_remove_part, deco, 1);
  must_be_number(s_deco_remove_part, num, 2);
  m = WL_DECO(deco)->get_num_parts();
  n = gh_scm2int(num);
  if (m == -1)
    gwm_misc_error(s_deco_remove_part, "Deco ~A can have no parts", deco);
  if (n<1 || n>m)
    scm_out_of_range_pos(s_deco_remove_part, num, gh_int2scm(2));
  WL_DECO(deco)->remove_part(n);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_remove_from_parent, "deco-remove-from-parent!", 1, 0, 0,
           (SCM deco),
           "Remove decoration from its parent.")
{
  Decoration* par;
  must_be_deco(s_deco_remove_from_parent, deco, 1);
  par = WL_DECO(deco)->Parent();
  if (par)
    par->remove_part(WL_DECO(deco));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_reorder_part, "deco-reorder-part!", 3, 0, 0,
           (SCM deco, SCM num1, SCM num2),
           "Reorder the sub-decorations of deco by moving the one at position num1 to num2.")
{
  int n1, n2, m;
  must_be_deco(s_deco_reorder_part, deco, 1);
  must_be_number(s_deco_reorder_part, num1, 2);
  must_be_number(s_deco_reorder_part, num2, 3);
  m = WL_DECO(deco)->get_num_parts();
  n1 = gh_scm2int(num1);
  n2 = gh_scm2int(num2);
  if (m == -1)
    gwm_misc_error(s_deco_reorder_part, "Deco ~A can have no parts", deco);
  if (n1<1 || n1>m)
    scm_out_of_range_pos(s_deco_reorder_part, num1, gh_int2scm(2));
  if (n2<1 || n2>m)
    scm_out_of_range_pos(s_deco_reorder_part, num2, gh_int2scm(3));
  WL_DECO(deco)->reorder_part(n1, n2);
  return SCM_UNSPECIFIED;
}


/* absolute position (% screen) of current deco
 */

SCM_DEFINE(deco_x_get, "deco-x", 1, 0, 0,
           (SCM deco),
           "Get x position of deco (relative the screen).")
{
  Decoration* w;
  int x;
  must_be_deco(s_deco_x_get, deco, 1);
  w = WL_DECO(deco);
  x = w->Xpos();
  while (w->Parent()) {
    w = w->Parent();
    if (!(w->Type() & InternMenuStatus))
      x += w->Borderwidth() + w->Xpos();
  }
  return gh_int2scm(x);
}

SCM_DEFINE(deco_y_get, "deco-y", 1, 0, 0,
           (SCM deco),
           "Get y position of deco (relative the screen).")
{
  Decoration* w;
  int y;
  must_be_deco(s_deco_y_get, deco, 1);
  w = WL_DECO(deco);
  y = w->Ypos();
  while (w->Parent()) {
    w = w->Parent();
    if (!(w->Type() & InternMenuStatus))
      y += w->Borderwidth() + w->Ypos();
  }
  return gh_int2scm(y);
}

/* move through deco hierarchy */

SCM_DEFINE(get_deco_parent, "deco-parent", 1, 0, 0,
           (SCM deco),
           "Get parent of deco.")
{
  must_be_deco(s_get_deco_parent, deco, 1);
  if (WL_DECO(deco)->Parent())
    return WL_DECO(deco)->Parent()->scm();
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(get_deco_top, "top-deco", 1, 0, 0,
           (SCM win),
           "Get the top-most parent of the given decoration.")
{
  must_be_deco(s_get_deco_top, win, 1);
  return WL_DECO(win)->Top()->scm();
}

SCM_DEFINE(deco_is_window, "deco-window?", 1, 0, 0,
           (SCM deco),
           "Check if deco is part of a client window decoration.")
{
  Decoration* d;
  must_be_deco(s_deco_is_window, deco, 1);
  d = WL_DECO(deco);
  if (d->Type() & WindowStatus ||
      (d->Type() == ClientStatus && d->Win() && d == d->Win()->Client()))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(deco_is_icon, "deco-icon?", 1, 0, 0,
           (SCM deco),
           "Check if deco is part of an icon decoration.")
{
  Decoration* d;
  must_be_deco(s_deco_is_icon, deco, 1);
  d = WL_DECO(deco);
  if (d->Type() & IconStatus ||
      (d->Type() == ClientStatus && d->Win() && d != d->Win()->Client()))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(deco_is_menu, "deco-menu?", 1, 0, 0,
           (SCM deco),
           "Check if deco is part of a menu (or inner part of a placed menu).")
{
  must_be_deco(s_deco_is_menu, deco, 1);
  if (WL_DECO(deco)->Type() & MenuStatus)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(deco_is_screen, "deco-screen?", 1, 0, 0,
           (SCM deco),
           "Check if deco is a screen decoration.")
{
  must_be_deco(s_deco_is_screen, deco, 1);
  if (WL_DECO(deco)->Type() & ScreenStatus)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(hide_deco, "hide-deco", 1, 0, 0,
           (SCM deco),
           "Make deco invisible. This can also be used to unmap a window.")
{
  must_be_deco(s_hide_deco, deco, 1);
  WL_DECO(deco)->hide();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(show_deco, "show-deco", 1, 0, 0,
           (SCM deco),
           "Make a previously invisible deco visible.")
{
  must_be_deco(s_show_deco, deco, 1);
  WL_DECO(deco)->show();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(deco_is_hidden, "deco-hidden?", 1, 0, 0,
           (SCM deco),
           "Check if deco is made invisible with 'hide-deco'.")
{
  must_be_deco(s_deco_is_hidden, deco, 1);
  return (WL_DECO(deco)->Hidden() == 1 ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM_DEFINE(force_show_window, "force-show-window", 1, 0, 0,
           (SCM window),
           "Make sure that the window (or icon) is mapped regardless of iconification state.")
{
  must_be_valid_window(s_force_show_window, window, 1);
  WL_DECO(window)->force_show();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(unforce_show_window, "unforce-show-window", 1, 0, 0,
           (SCM window),
           "Remove the effect of 'force-show-window'.")
{
  must_be_valid_window(s_unforce_show_window, window, 1);
  WL_DECO(window)->unforce_show();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(window_is_mapped, "window-mapped?", 1, 0, 0,
           (SCM window),
           "Check if window is mapped.")
{
  must_be_window(s_window_is_mapped, window, 1);
  if (WL_DECO(window)->Type() & IconStatus ?
      WL_WINDOW(window)->MappedIcon() :
      WL_WINDOW(window)->MappedWin())
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(deco_is_mapped, "deco-mapped?", 1, 0, 0,
           (SCM deco),
           "Check if deco is visible, i.e. it and all parents should be mapped.")
{
  must_be_deco(s_deco_is_mapped, deco, 1);
  return (WL_DECO(deco)->Visible() ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM_DEFINE(deco_is_shaped, "deco-shaped?", 1, 0, 0,
           (SCM deco),
           "Check if deco has a nonrectangular shape.")
{
  must_be_deco(s_deco_is_shaped, deco, 1);
  if (WL_DECO(deco)->Shaped())
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(LookUpDecoW, "xid->deco", 1, 0, 0,
           (SCM id),
           "Find decoration from its X id.")
{
  Decoration* deco;
  must_be_number(s_LookUpDecoW, id, 1);
  if ((deco = LookUpDeco(gh_scm2int(id))))
    return deco->scm();
  else
    return SCM_BOOL_F;
}

SCM_DEFINE(Client2WindowW, "client-xid->window", 1, 0, 0,
           (SCM clientid),
           "Find a window from the X id of its client.")
{
    ClientWindow* cw;

    must_be_number(s_Client2WindowW, clientid, 1);
    if ((cw = LookUpClient((Window) gh_scm2int(clientid)))) {
	return cw->Deco()->scm();
    } else {
	return SCM_BOOL_F;
    }
}

SCM_DEFINE(Window2ClientW, "window->client-xid", 1, 0, 0,
           (SCM window),
           "Get the X id corresponding to the client window.")
{
    ClientWindow* cw;

    must_be_window(s_Window2ClientW, window, 1);
    cw = WL_WINDOW(window);
    if (cw->Client()) {
	return gh_int2scm(cw->InnerWin());
    } else {
	return SCM_BOOL_F;
    }
}

void UpdateMenuHints(Decoration* menu, int pos, int x, int y, SCM ctx, int cn, const char* subr)
{
  XSizeHints normal_hints;
  XClassHint classhints;
  //  XWMHints wm_hints;
  char* string;
  SCM grav, name;

  normal_hints.min_width = normal_hints.max_width = menu->Width() + 2*menu->Borderwidth();
  normal_hints.min_height = normal_hints.max_height = menu->Height() + 2*menu->Borderwidth();
  if (pos) {
    normal_hints.x = x;
    normal_hints.y = y;
  }
  grav = gwm_get_keyword(k_gravity, ctx, cn, SCM_UNDEFINED);
  normal_hints.win_gravity = (grav != SCM_UNDEFINED ? gwm_decode_gravity(grav) : StaticGravity);
  normal_hints.flags = (pos ? USPosition : 0) | PMinSize | PMaxSize | PWinGravity; 
  XSetWMNormalHints(dpy, menu->Xwin(), &normal_hints);
  /* Now, set the names */
  name = gwm_get_keyword(k_name, ctx, cn, scm_makfrom0str(""));
  string = wl_getstring(name, subr);
  XStoreName(dpy, menu->Xwin(), string);
  delete [] string;
  string = wl_getstring(gwm_get_keyword(k_icon_name, ctx, cn, name), subr);
  XSetIconName(dpy, menu->Xwin(), string);
  delete [] string;
  string = wl_getstring(gwm_get_keyword(k_class_name, ctx, cn, scm_makfrom0str("Gwm")), subr);
  classhints.res_class = string;
  string = wl_getstring(gwm_get_keyword(k_client_name, ctx, cn, scm_makfrom0str("menu")), subr);
  classhints.res_name = string;
  XSetClassHint(dpy, menu->Xwin(), &classhints);
  delete [] classhints.res_class;
  delete [] classhints.res_name;
  /* machine_name */
  XChangeProperty(dpy, menu->Xwin(), XA_WM_CLIENT_MACHINE, XA_STRING, 8, PropModeReplace,
                  (unsigned char*) scm_i_string_chars(GWM_host_name),
                  scm_i_string_length(GWM_host_name));
  /* WM_hints */
  //  wm_hints.flags = 0;
  //  XSetWMHints(dpy, menu->Xwin(), &wm_hints);
  /* participate in the delete_window protocol */
  {
#define GWM_menus_number_of_protocols 1
    Window protocols[GWM_menus_number_of_protocols];
    protocols[0] = XA_WM_DELETE_WINDOW;
    XChangeProperty(dpy, menu->Xwin(), XA_WM_PROTOCOLS, XA_ATOM, 32, PropModeReplace,
                    (unsigned char *)protocols, GWM_menus_number_of_protocols);
  }
}

/*
 * Place a fixed menu on the screen
 * usage (place-menu "name-of-this-menu" menu [x y])
 */

SCM_DEFINE(PlaceFixedMenu, "place-menu", 2, 0, 1,
           (SCM deco, SCM screen, SCM args),
           "Place deco (previously realized or not) as a fixed menu on screen at"
           "optional coordinates x and y. Possible keys are:"
           ":name            Menu windows name"
           ":icon-name       Menu windows icon name"
           ":class-name      Menu windows client class name"
           ":client-name     Menu windows client name"
           ":gravity         Gravity of menu window"
           ":decoration      Special decoration function instead of 'describe-window'"
           ":icon-decoration Special icon decoration function instead of 'describe-icon'")
{
  int x = 0, y = 0;
  Decoration* wob;
  ClientWindow* cw;
  SCM proc, iproc;
  SCM ctx, sub;
  int n, cn;
  XSetWindowAttributes wa;

  must_be_deco(s_PlaceFixedMenu, deco, 1);
  if (!WLSCREENP(screen))
    gwm_wrong_type_arg(s_PlaceFixedMenu, 2, screen, "screen");
  n = wl_separate_context(args, sub, ctx, cn, s_PlaceFixedMenu);
  if (n == 2) {
    must_be_number(s_PlaceFixedMenu, SCM_CAR(sub), 3);
    must_be_number(s_PlaceFixedMenu, SCM_CAR(SCM_CDR(sub)), 4);
    x = gh_scm2int(SCM_CAR(sub));
    y = gh_scm2int(SCM_CAR(SCM_CDR(sub)));
  } else if (n != 0)
    gwm_wrong_num_args(s_PlaceFixedMenu, n+2);
  wob = WL_DECO(deco);
  wob->assure_free_deco(s_PlaceFixedMenu, 1);
  if (wob->Type() == NoStatus)
    wob = new IMenu(wob, WL_SCREEN(screen));
  else if (wob->check_free_menu()) {
    if (wob->Screen() != WL_SCREEN(screen)) {
      wob = ((IMenu*)wob)->unrealize();
      wob = new IMenu(wob, WL_SCREEN(screen));
    } else
      XRaiseWindow(dpy, wob->Xwin()); // Make sure it opens up on top
  } else
    gwm_misc_error(s_PlaceFixedMenu, "Deco can not be made a menu: ~A", deco);
  proc = gwm_get_keyword(k_decoration, ctx, cn, SCM_UNDEFINED);
  if (proc != SCM_UNDEFINED) {
    if (!gh_procedure_p(proc))
      gwm_wrong_type_arg(s_PlaceFixedMenu, 0, proc, "procedure");
  } else
    proc = 0;
  iproc = gwm_get_keyword(k_icon_decoration, ctx, cn, SCM_UNDEFINED);
  if (iproc != SCM_UNDEFINED) {
    if (!gh_procedure_p(iproc))
      gwm_wrong_type_arg(s_PlaceFixedMenu, 0, iproc, "procedure");
  } else
    iproc = 0;
  ((IMenu*) wob)->SetDecoProcedure(proc);
  ((IMenu*) wob)->SetIconProcedure(iproc);
  ((InnerDeco*) wob)->SetOrigPos(x, y);
  UpdateMenuHints(wob, (n == 2 ? 1 : 0), x, y, ctx, cn, s_PlaceFixedMenu);
  wa.override_redirect = 0;
  XChangeWindowAttributes(dpy, wob->Xwin(), CWOverrideRedirect, &wa);

  /* now decorate it and map it */
  cw = DecorateWindow(wob->Xwin(), wob->Screen(), proc, 1);
  if (!cw)
    gwm_misc_error(s_PlaceFixedMenu, "Failed to decorate menu ~A", deco);
  XSync(dpy, 0);
  return cw->Deco()->scm();
}

/* returns SCM_UNSPECIFIED if OK
 * (pop-up [menu [item]])
 * if menu = () takes default
 * item is the nth item in the menu (starting with 0).
 * If pointer is not on screen, do nothing and returns 0
 * if cannot grab, returns 1, Ok= ()
 * returns pointer (modifier + buttons status) mask
 */

SCM_DEFINE(PopMenu, "pop-menu", 2, 0, 1,
           (SCM deco, SCM screen, SCM args),
           "Open deco as a popup menu on screen at optional coordinates (x,y) and"
           "let it grab the server. Possible keys are:"
           ":grab-keyboard   keyboard events are also grabbed"
           ":confine-pointer confine pointer within deco"
           ":menu-parent     decoration which popped this menu and keeps the grab"
           ":cursor          cursor to use during the grab")
{
  int x, y, rx, ry;
  Window root, child;
  Decoration* menu;
  int pointer = 1;
  unsigned int mask;
  SCM sub, ctx;
  SCM cursor, gpar, kbddef;
  int kbd, cnf, nofrz;
  int argc, cn;

  must_be_deco(s_PopMenu, deco, 1);
  if (!WLSCREENP(screen))
    gwm_wrong_type_arg(s_PopMenu, 2, screen, "screen");
  argc = wl_separate_context(args, sub, ctx, cn, s_PopMenu);
  if (argc == 2) {
    must_be_number(s_PopMenu, SCM_CAR(sub), 2);
    must_be_number(s_PopMenu, SCM_CAR(SCM_CDR(sub)), 3);
    x = gh_scm2int(SCM_CAR(sub));
    y = gh_scm2int(SCM_CAR(SCM_CDR(sub)));
    pointer = 0;
  } else if (argc != 0)
    gwm_wrong_num_args(s_PopMenu, argc+2);
  nofrz = wl_getbool(gwm_get_keyword(k_no_freeze, ctx, cn, SCM_BOOL_F), s_PopMenu);
  cnf = wl_getbool(gwm_get_keyword(k_confine_pointer, ctx, cn, SCM_BOOL_F), s_PopMenu);
  gpar = gwm_get_keyword(k_menu_parent, ctx, cn, SCM_UNDEFINED);
  menu = WL_DECO(deco);
  menu->assure_free_deco(s_PopMenu, 1);
  if (menu->Type() == NoStatus)
    menu = new IMenu(menu, WL_SCREEN(screen));  // It wont be lost, it hooks itself up in deco
  else if (menu->check_free_menu()) {
    if (menu->Screen() != WL_SCREEN(screen)) {
      menu = ((IMenu*)menu)->unrealize();
      menu = new IMenu(menu, WL_SCREEN(screen));
    }
  } else
    gwm_misc_error(s_PopMenu, "Deco can not be made a menu: ~A", deco);
  kbddef = (menu->MaybeMenu()->GetFsm()->CanUseFocus() ? SCM_BOOL_T : SCM_BOOL_F);
  kbd = wl_getbool(gwm_get_keyword(k_grab_keyboard, ctx, cn, kbddef), s_PopMenu);
  cursor = gwm_get_keyword(k_cursor, ctx, cn, menu->get_cursor());
  if (pointer) {
    XQueryPointer(dpy, menu->Screen()->Root(), &root, &child,
                  &x, &y, &rx, &ry, &mask);
    if (root != menu->Screen()->Root())
      x = 0, y = 0;
  }
  ((IMenu*)menu)->pop(x, y,
                      (WLDECOP(gpar) ? WL_DECO(gpar) : 0),
                      (WLCURSORP(cursor) ? WL_CURSOR(cursor) : 0),
                      kbd, 0, cnf, nofrz);
  return SCM_UNSPECIFIED;
}

/* de-pop a menu previously popped by PopMenu
 */

SCM_DEFINE(UnpopMenu, "unpop-menu", 1, 0, 0,
           (SCM menu),
           "Unpop the menu and ungrab the server.")
{
  Decoration* mn = (WLDECOP(menu) ? WL_DECO(menu) : 0);
  if (mn && (mn->Type() == MenuStatus) && (mn->Valid() == 3))
    ((IMenu*)mn)->unpop();
  else
    gwm_wrong_type_arg(s_UnpopMenu, 1, menu, "popped menu");
  return menu;
}

void init_scm_decofunc()
{
#include "decofunc.x"
}
