# Revision history for WEditor

## 0.2.0.0  -- 2020-04-17

* **[breaking]** Adds line-splitting to `FixedFontParser` requirements. This
  ensures that a custom parser that formats the front of the line (e.g., adding
  block indentation) works properly when paragraph breaks are inserted. This
  does not affect compilation or behavior of code that isn't using custom
  line-wrapping policies.

* **[breaking]** Adds vertical shifting of the viewable text area. This only
  affects compilation of custom `FixedFontViewer`, and otherwise extends the
  available functionality of existing code.

## 0.1.0.0  -- 2020-04-14

* First version. Released on an unsuspecting world.
