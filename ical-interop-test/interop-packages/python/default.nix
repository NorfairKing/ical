{ python3Packages
}:
with python3Packages;
buildPythonApplication {
  pname = "python-echo";
  version = "0.0";
  propagatedBuildInputs = [ icalendar ];

  src = ./.;
}
