with WL.String_Maps;

with Hera.UI.Models.Shell;

with Hera.UI.Models.Galaxy;
with Hera.UI.Models.Planets;

--  with Hera.UI.Models.Star_System;
--  with Hera.UI.Models.Worlds;
--  with Hera.UI.Models.World_Ships;

package body Hera.UI.Models.Loader is

   package Model_Maps is
     new WL.String_Maps (Root_Hera_Model'Class);

   Map : Model_Maps.Map;

   procedure Check_Map;

   ---------------
   -- Check_Map --
   ---------------

   procedure Check_Map is

      procedure Add
        (Name    : String;
         Model   : Root_Hera_Model'Class);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name  : String;
         Model : Root_Hera_Model'Class)
      is
      begin
         Map.Insert (Name, Model);
      end Add;

   begin
      if Map.Is_Empty then
         Add ("shell", Hera.UI.Models.Shell.Shell_Model);
         Add ("galaxy",
              Hera.UI.Models.Galaxy.Galaxy_Model);
         Add ("planet",
              Hera.UI.Models.Planets.Planet_Model);

--           Add ("star-system",
--                Hera.UI.Models.Star_System.Star_System_Model);
--           Add ("world",
--                Hera.UI.Models.Worlds.World_Model);
--           Add ("world-ships",
--                Hera.UI.Models.World_Ships.World_Ship_Model);
      end if;
   end Check_Map;

   ------------
   -- Exists --
   ------------

   function Exists (Model_Name : String) return Boolean is
   begin
      Check_Map;
      return Map.Contains (Model_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Model_Name : String)
      return Hera_Model
   is
   begin
      Check_Map;
      return Model : constant Hera_Model :=
        new Root_Hera_Model'Class'
          (Map.Element (Model_Name));
   end Get;

end Hera.UI.Models.Loader;
