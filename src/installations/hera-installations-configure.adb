package body Hera.Installations.Configure is

   --------------------------
   -- Initial_Installation --
   --------------------------

   procedure Initial_Installation
     (Owner    : Hera.Corporations.Corporation_Type;
      Colony   : Hera.Colonies.Colony_Type;
      Facility : Hera.Facilities.Facility_Type)
   is
   begin
      Create (Owner, Colony, Facility);
   end Initial_Installation;

end Hera.Installations.Configure;
