// Firefox customization
// Inspired from https://github.com/pyllyukko/user.js

// Installation
// ln -sv ~/.dotfiles/appdesktop/.config/firefox/user.js ~/.mozilla/firefox/XXXXXXXX.default-release/user.js
// ln -sv ~/.dotfiles/appdesktop/.config/firefox/user.js ~/.mozilla/firefox/XXXXXXXX.dev-edition-default/user.js

// Making the UI, toolbars etc. smaller
user_pref("browser.compactmode.show", true);
user_pref("browser.uidensity", 1);

// Enables using the urlbar as calculator and for unit conversion
user_pref("browser.urlbar.unitConversion.enabled", true);
user_pref("browser.urlbar.suggest.calculator", true);

// The window doesn't close when closing the last tab
// user_pref("browser.tabs.closeWindowWithLastTab", true);

// Show "http://"
user_pref("browser.urlbar.trimURLs", false);
// Don't try to guess domain names when entering an invalid domain name in URL bar
user_pref("browser.fixup.alternate.enabled", false);

// Select all urlbar on click
// user_pref("browser.urlbar.clickSelectsAll", true);

// Warning if CTRL + Q
user_pref("browser.showQuitWarning", true);

// Devtools
user_pref("devtools.command-button-eyedropper.enabled", true);
user_pref("devtools.command-button-screenshot.enabled", true);
user_pref("devtools.editor.tabsize", 4);

// Plugin installation delay
user_pref("security.dialog_enable_delay", 0);

// Do not steal focus
user_pref("browser.tabs.loadDivertedInBackground", true);
// Load tabs in bacground
user_pref("browser.tabs.loadBookmarksInBackground", true);

// Disable the UITour backend
user_pref("browser.uitour.enabled", false);

// Prevent F11 or fullscreen video from fullscreening the window
user_pref("full-screen-api.ignore-widgets", true);
user_pref("full-screen-api.exit-on.windowRaise", true);
user_pref("full-screen-api.exit-on.windowOpen", true);

// Disable animation
user_pref("full-screen-api.transition-duration.enter", "0 0");
user_pref("full-screen-api.transition-duration.leave", "0 0");
user_pref("full-screen-api.transition.timeout", 0);

// Remove fullscreen warning
user_pref("full-screen-api.warning.delay", 0);
user_pref("full-screen-api.warning.timeout", 0);

/******************************************************************************
 * Lang
 ******************************************************************************/
// Disable GeoIP lookup on your address to set default search engine region
// user_pref("browser.search.countryCode", "US");
// user_pref("browser.search.region", "US");
// user_pref("browser.search.geoip.url", "");

// Set Accept-Language HTTP header to en-US regardless of Firefox localization
// user_pref("intl.accept_languages", "en-US, en");

// Don't use OS values to determine locale, force using Firefox locale setting
// http://kb.mozillazine.org/Intl.locale.matchOS
// user_pref("intl.locale.matchOS", false);

// Don't use Mozilla-provided location-specific search engines
// user_pref("browser.search.geoSpecificDefaults", false);


/******************************************************************************
 * Privacy
 ******************************************************************************/
// Do not track
user_pref("privacy.donottrackheader.enabled", true);
// Disable Location-Aware Browsing
// user_pref("geo.enabled", false);
// Disable Safe browsing
// user_pref("browser.safebrowsing.enabled", false);
// user_pref("browser.safebrowsing.phishing.enabled", false);
// Safe browsing URL
// user_pref("browser.safebrowsing.appRepURL", '');
// Safe browsing malware
// user_pref("browser.safebrowsing.malware.enabled", false);

// Disable Mozilla telemetry/experiments
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("experiments.supported", false);
user_pref("experiments.enabled", false);
user_pref("experiments.manifest.uri", "");

// Disable collection/sending of the health report (healthreport.sqlite*)
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.healthreport.service.enabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
// "Allow Firefox to make personalized extension recommendations"
user_pref("browser.discovery.enabled", false);

// Disable Shield/Heartbeat/Normandy (Mozilla user rating telemetry)
user_pref("app.normandy.enabled", false);
user_pref("app.normandy.api_url", "");
user_pref("extensions.shield-recipe-client.enabled", false);
user_pref("app.shield.optoutstudies.enabled", false);

// Disable Pocket
user_pref("browser.pocket.enabled", false);
user_pref("extensions.pocket.enabled", false);
// Disable "Recommended by Pocket" in Firefox Quantum
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
// Disable Extension recommendations (Firefox >= 65)
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr", false);
// Disable "TopSites"
user_pref("browser.topsites.contile.enabled", false);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
// Disable Snippets
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);
// Disable Activity Stream
user_pref("browser.newtabpage.activity-stream.enabled", false);
// PREF: Disable Mozilla VPN ads on the about:protections page
user_pref("browser.vpn_promo.enabled", false);

// Disable battery API
user_pref("dom.battery.enabled", false);
// Disable telephony API
user_pref("dom.telephony.enabled", false);
// Disable face detection
user_pref("camera.control.face_detection.enabled", false);
