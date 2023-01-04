{ inputs }:

[
  { ghcVersion = "88"; }
  { ghcVersion = "90"; }
  { ghcVersion = "92"; }
  {
    ghcVersion = "94";
    overrides = self: super: {
      # we might need some fixes for 9.4
      # a common example are libraries that have not yet updated version bounds for `base`
      # but still build otherwise
      # we can patch libraries individually
    };
  }
]
