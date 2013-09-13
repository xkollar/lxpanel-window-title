/**
 *
 * Copyright (c) 2009 LxDE Developers, see the file AUTHORS for details.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib/gi18n.h>

#include <string.h>

#include <lxpanel/plugin.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define my_COLOR 0x000000

typedef struct {
    Plugin * plugin;
    GtkWidget *label;
    Window * current_window;
    char * desktop_name;
    char * window_title;
    Atom title_source;
    guint32 * color;
} WindowTitle;

static void my_update_current_window_title(WindowTitle * egz)
{
    char * title = NULL;
    Atom title_source = None;
    Window * f = get_xaproperty(gdk_x11_get_default_root_xwindow(),
        a_NET_ACTIVE_WINDOW, XA_WINDOW, 0);

    if (f != NULL) {
        title = get_utf8_property(*f, a_NET_WM_VISIBLE_NAME);

        if (title == NULL) {
            title = get_utf8_property(*f, a_NET_WM_NAME);
        } else { 
            title_source = a_NET_WM_VISIBLE_NAME;
        }

        if (title == NULL) {
            title = get_textproperty(*f, XA_WM_NAME);
        } else { 
            title_source = a_NET_WM_NAME;
        }

        if (title != NULL) {
            title_source = XA_WM_NAME;
        }
    }

    Window * old_window = egz->current_window;
    egz->current_window = f;
    if (old_window != NULL) {
        XFree(old_window);
    }

    egz->title_source = title_source;

    char * old_window_title = egz->window_title;
    egz->window_title = title;
    g_free(old_window_title);
}

static void my_update_current_desktop_name(WindowTitle * egz)
{
    int number_of_desktop_names;
    char * *  desktop_names = get_utf8_property_list(
        gdk_x11_get_default_root_xwindow(),
        a_NET_DESKTOP_NAMES,
        &number_of_desktop_names);
    if (desktop_names == NULL) {
        return NULL;
    }
    int current_desktop = get_net_current_desktop();
    char * ret = NULL;

    if (current_desktop < number_of_desktop_names) {
        ret = g_strdup(desktop_names[current_desktop]);
    } else {
        fprintf(stderr, "Faaaak...\n");
    }
    g_strfreev(desktop_names);

    char * old = egz->desktop_name;
    egz->desktop_name = ret;
    g_free(old);
}

static void my_update_main_label(WindowTitle * egz)
{
    if (egz->desktop_name == NULL) {
        gtk_label_set_markup(GTK_LABEL(egz->label), "No desktop. Is window manager running?");
        return;
    }
    gchar * formated = NULL;
    if (egz->window_title == NULL) {
        formated = g_markup_printf_escaped("<span color=\"#%06x\">%s</span>",
            egz->color, egz->desktop_name); 
    } else {
        formated = g_markup_printf_escaped("<span color=\"#%06x\">%s: %s</span>",
            egz->color, egz->desktop_name, egz->window_title);
    }
    gtk_label_set_markup(GTK_LABEL(egz->label), formated);
    g_free(formated);
}

static void my_update_title_event(GtkWidget *widget, WindowTitle* egz)
{
    my_update_current_window_title(egz);
    my_update_main_label(egz);
}

static void my_update_desktop_event(GtkWidget *widget, WindowTitle* egz)
{
    my_update_current_desktop_name(egz);
    my_update_main_label(egz);
}


static int windowtitle_constructor(Plugin *p, char** fp)
{
    WindowTitle *egz;

    ENTER;
    /* initialization */
    egz = g_new0(WindowTitle, 1);
    egz->plugin = p;
    p->priv = egz;

    if (p->panel->usefontcolor) {
        egz->color = gcolor2rgb24(&p->panel->gfontcolor);
    } else {
        egz->color = my_COLOR;
    }

    my_update_current_desktop_name(egz);

    p->pwid = gtk_event_box_new();

    GTK_WIDGET_SET_FLAGS(p->pwid, GTK_NO_WINDOW);
    // gtk_container_set_border_width(GTK_CONTAINER(p->pwid), 2);

    egz->label = gtk_label_new(NULL);

    // gtk_label_set_justify(GTK_LABEL(egz->label), GTK_JUSTIFY_LEFT);
    // gtk_misc_set_alignment(GTK_MISC(egz->label), 0, .5);

    gtk_container_add(GTK_CONTAINER(p->pwid), egz->label);

    // gtk_label_set_markup(GTK_LABEL(egz->label), "Welcome...");
    gtk_label_set_text(GTK_LABEL(egz->label), "Welcome...");

    gtk_widget_show(egz->label);

    // char * kwak = "the most evil string<b>AAAA</b>";
    // gchar * utf8 = g_locale_to_utf8(kwak, -1, NULL, NULL, NULL);
    // gchar * formated = g_markup_printf_escaped("<span color=\"#ff0000\">%s</span>", utf8);
    // gtk_label_set_markup(GTK_LABEL(egz->label), formated);
    // // panel_draw_label_text(egz->plugin->panel, egz->label, utf8, TRUE, 1, TRUE);
    // g_free(formated);
    // g_free(utf8);

    my_update_main_label(egz);

    g_signal_connect (G_OBJECT (fbev), "active_window", G_CALLBACK (my_update_title_event), (gpointer) egz);
    g_signal_connect (G_OBJECT (fbev), "client_list", G_CALLBACK (my_update_title_event), (gpointer) egz);
    g_signal_connect (G_OBJECT (fbev), "client_list_stacking", G_CALLBACK (my_update_title_event), (gpointer) egz);

    g_signal_connect (G_OBJECT (fbev), "current_desktop", G_CALLBACK (my_update_desktop_event), (gpointer) egz);
    g_signal_connect (G_OBJECT (fbev), "desktop_names", G_CALLBACK (my_update_desktop_event), (gpointer) egz);
    g_signal_connect (G_OBJECT (fbev), "number_of_desktops", G_CALLBACK (my_update_desktop_event), (gpointer) egz);

    // gtk_widget_show(egz->label); /* show plugin on panel */
    RET(TRUE);
error:
    destructor(p);
    RET(FALSE);
}

static void windowtitle_destructor(Plugin *p)
{
    ENTER;
    WindowTitle *egz = (WindowTitle *)p->priv;

    g_signal_handlers_disconnect_by_func(fbev, my_update_desktop_event, egz);
    g_signal_handlers_disconnect_by_func(fbev, my_update_title_event, egz);

    g_free(egz->desktop_name);
    g_free(egz->window_title);
    if (egz->current_window) {
        XFree(egz->current_window);
    }
    gtk_widget_destroy(egz->label);
    g_free(egz);
    RET();
}

PluginClass windowtitle_plugin_class = {
    PLUGINCLASS_VERSIONING,

    type : "windowtitle",
    name : N_("Window title"),
    version: "0.1",
    description : N_("Show name of current desktop + title of active window if any."),

    expand_available : TRUE,

    constructor : windowtitle_constructor,
    destructor  : windowtitle_destructor,
};
// vim:set expandtab tabstop=4 shiftwidth=4:
