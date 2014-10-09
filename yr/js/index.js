/* See end for application documentation */

window.addEventListener('DOMContentLoaded', SplashScreenLogic);

const default_version = "1";
const default_forecast_lang = "Nynorsk";
const default_display_lang = "no-nn";
const default_locations = [];

var errors = false;

function SplashScreenLogic() {
   var then = new Date();
   try {
      InitSettings();
   }
   catch (e) {
      alert("Database error!");
      if (!errors) {
         errors = true;
         localStorage.removeItem("version");
         localStorage.removeItem("display-lang");
         localStorage.removeItem("forecast-lang");
         localStorage.removeItem("locations");
         SplashScreen();
      }
      return;
   }
   var now = new Date();
   if (now - then < 1000)
       setTimeout(GotoApp, 1000 - (now - then)));
   else
      GotoApp();
}

function GotoApp() {
   sessionStorage.setItem("location", "0");
   var numlocs = JSON.parse(localStorage.getItem("locations")).length;
   window.location.replace(numlocs > 0 ? "/location.html" : "/settings.html");
}

function InitSettings() {
   var v = localStorage.getItem("version");
   if (!v || parseInt(v).toString() != v) {
      localStorage.setItem("version", default_version);
   var fl = localStorage.getItem("forecast-lang");
   if (!fl || !(fl in { "Norsk":1, "Nynorsk":1, "English":1 }))
     localStorage.setItem("forecast-lang", default_forecast_lang);
   var dl = localStorage.getItem("display-lang");
   if (!dl)
      localStorage.setItem("display-lang", default_display_lang);
   var loc = localStorage.getItem("locations");
   if (loc) {
      try {
         loc = JSON.parse(loc);
         if (!(loc instanceof Array))
            loc = null;
         else {
            for ( var l of loc )
               if (!("country" in l && "place" in l)) {
                  loc = null;
                  break;
               }
         }
      }
      catch (e) {}
   }
   if (!loc)
      localStorage.setItem("locations", JSON.stringify(default_locations));
}

/*
Interaction:

index.html is the splash screen showing a logo
  - in the future it will load localizations
  - in the future it may initialize the permanent store of place names
  - we do database integrity checks here and create it if it doesn't exist
  - when the splash screen is done (minimum 1 second):
      - if there are any locations then load location.html on the first page in the list
      - if there are no locations then load settings.html

location.html displays the weather for a location in the location list
  - (failsafe) if no locations in the list goto settings.html
  - the location index is given in the session store
  - (failsafe) if location index is invalid (missing / not integer / out of range), set it to zero; retry from top
  - we get the forecast from the session store
  - if the forecast is more than 10 minutes old we reload it, but:
    - if it can't be reloaded (network error, whatever)
       - use a cached value if available
       - display an in-page "No data available" error, do not abort or delete site
  - long-term preview broken down by time
  - swipe right:
     - if not at last location then go to location.html with the next location
     - if at last location then go to settings.html
  - swipe left:
     - if not at first location then go to location.html with the previous location
     - if at first location then go to settings.html
  
settings.html displays all the locations in a compressed list, has a search button for
   new locations, and has selectors for language and maybe other things
  - long-click a location to get a question about whether to remove, move up, move down,
    move to top
     - remove: remove it from the list
     - move up: swap with previous
     - move down: swap with next
     - move to top: rotate into top spot
  - select language from language dropdown
  - click the search button to go to search.html
  - swipe left:
     - if location list is not empty go to location.html with last page in list
     - if location list is empty do nothing
  - swipe right:
     - if location list is not empty go to location.html with first page in list
     - if location list is empty do nothing

search.html has a text box for searching and a search button and a back button.
  - Back button (why?) takes us back to settings.html (at selected location) without doing anything.
  - Type a search term and press the search button.  This will yield a list of matches
    in a list or "No matches".  Select a match to include it at the end of the list of
    locations and go back to location.html showing that location.

(For prototyping purposes there can be left/right buttons at the top of
 locations.html and settings.html to scroll.  I don't know how much has
 to be handled manually.  Some older stuff here, probably not applicable:
 https://developer.mozilla.org/en-US/docs/Web/Guide/Events/Mouse_gesture_events)



Databases:

We use localStorage for settings:

Key             Value
---             -----
version         String, currently only "1"
forecast-lang   String, one of "Norsk", "Nynorsk", "English"
display-lang    String, a valid localization key (eg "es", "en-US", "no-nn")
locations       JSON data "Locs"

Locs    ::= Array of Loc
Loc     ::= Object with keys "country", "place""
Country ::= string: two-letter country code
Place   ::= string: slash-separated triple of region/subregion/name, each element URL-encoded


We use sessionStorage for application state (globals):

Key        Value
---        -----
location   integer - index in the list of locations
<Loc>      JSON precis of the most recent forecast we have for the Loc triplet (TBD)


We use multiple text files for the store of known forecast location data:

There are data files whose names match the prefix of the search term in
canonical case; "/data/loc-XX.data".  If the place name is a single letter,
there will be a single-letter file "/data/loc-X.data".  Otherwise the
place name is in the file named with the prefix of the first two letters.

The format of the index file is a string where each entry looks like this:

   *text/k/co/region/subregion/place

where k is a code that indicates the type of place it is (TBD)
and co is the country code.  text is always in a canonical lower case form
but is otherwise the same as place.  Region, subregion, and place are
properly capitalized for display.  The entries within a file are sorted
lexicographically by 'text'.  No field contains '*'.

Optionally there is a LF or CRLF at the end of a record.

(If the files turn out to be large there are many simple compression
opportunities that will not complicate the basic search.)

*/
