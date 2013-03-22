#ifdef GSTREAMER
#include "gst/gst.h"
#include <stdbool.h>
#include <string.h>

// This is based on an example from:
// http://felipec.wordpress.com/2008/01/19/gstreamer-hello-world/
// Documentation on the various gst calls is located at
// http://gstreamer.freedesktop.org/data/doc/gstreamer/head/gstreamer/html/gstreamer-Gst.html
// Playbin is documented:
// http://gstreamer.freedesktop.org/data/doc/gstreamer/head/gst-plugins-base-plugins/html/gst-plugins-base-plugins-playbin.html
//
// Also, gst-launch URI does the same thing on the command line

// Compile as:
// $ gcc `pkg-config --cflags --libs gstreamer-0.10` c_gstreamer.c
//
// SDL also plays wav's (and most other files) but the mixer must be opened
// at specific rates.  Here we want arbitrary types so we're using gstreamer.
// However, SDL docs are here:
// http://content.gpwiki.org/index.php/SDL_mixer:Tutorials:Playing_a_WAV_Sound_File
     
static GMainLoop *loop;

char gst_error[256] = "\0";
     
static gboolean bus_call(GstBus *bus, GstMessage *msg, void *user_data) {
    switch (GST_MESSAGE_TYPE(msg)) {
      case GST_MESSAGE_EOS: {
        // g_message("End-of-stream");
        g_main_loop_quit(loop);
        break;
      }
      case GST_MESSAGE_ERROR: {
	GError *err;
        gst_message_parse_error(msg, &err, NULL);
	strncpy( gst_error, err->message, 256 );
	gst_error[255] = '\0';
	// For debugging:
        //g_error("%s", err->message);
        //g_error_free(err);
        g_main_loop_quit(loop);
        break;
      }
      default:
        break;
      }
     
      return true;
    }
     
// Plays a sound fine located at uri.  Return true if no error.
//
// playbin is a general gstreamer plugin for playing from a URI.
// It creates a pipeline and auto-detects properties like
// playback format.

    int play_uri(const char *uri) {
      GstElement *pipeline;
      GstBus *bus;

      gst_error[0] = '\0';
      loop = g_main_loop_new(NULL, FALSE);
      pipeline = gst_element_factory_make("playbin", "player");
 
      if (uri)
        g_object_set(G_OBJECT(pipeline), "uri", uri, NULL);
     
      bus = gst_pipeline_get_bus(GST_PIPELINE(pipeline));
      gst_bus_add_watch(bus, bus_call, NULL);
      gst_object_unref(bus);
     
      gst_element_set_state(GST_ELEMENT(pipeline), GST_STATE_PLAYING);
     
      g_main_loop_run(loop);
     
      gst_element_set_state(GST_ELEMENT(pipeline), GST_STATE_NULL);
      gst_object_unref(GST_OBJECT(pipeline));
      return gst_error[0] == '\0';
    }

    void startup_gstreamer() {
      gst_init( NULL, NULL );
    }

    void shutdown_gstreamer() {
      gst_deinit();
    }

/* 
    int main(int argc, char *argv[]) {
      int res;
      startup_gstreamer();
      res = play_uri( "file:///home/ken/ada/bush/examples/clap.wav" );
      // In Ada, check gst_error for an error message.
      return !res;
    }
*/
#endif

