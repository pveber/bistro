open Bistro

class type nhx = object
  inherit text
  method format : [`nhx]
end

class type newick = object
  inherit text
  method format : [`newick]
end

class type nexus = object
  inherit text
  method format : [`nexus]
end

class type phylip = object
  inherit text
  method format : [`phylip]
end
