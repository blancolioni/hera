with WL.String_Maps;

package body Hera.Climate is

   Climate_Version : constant Hera.Objects.Object_Version := "0.0.1";

   package Climate_Maps is
     new WL.String_Maps (Climate_Type);

   Map : Climate_Maps.Map;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Climate_Type is
   begin
      return Map.Element (Tag);
   end Get;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Climate : in out Root_Climate_Type;
      From      : Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Climate).Load (From);
   end Load;

   -------------------
   -- New_Climate --
   -------------------

   procedure New_Climate
     (Tag             : String;
      Hazard_Level    : Unit_Real;
      Default_Terrain : Hera.Terrain.Terrain_Type;
      Terrain_Chance  : Terrain_Chance_Lists.List)
   is
      Climate_Rec : constant Root_Climate_Type :=
                      Root_Climate_Type'
                        (Hera.Objects.Root_Localised_Object with
                         Hazard_Level    => Hazard_Level,
                         Default_Terrain => Default_Terrain,
                         Terrain_Chance  => Terrain_Chance);
   begin
      Map.Insert
        (Tag, Climate_Type
           (Climate_Rec.New_Localised_Object (Climate_Version, Tag)));
   end New_Climate;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Climate : Root_Climate_Type;
      To      : in out Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Climate).Save (To);
   end Save;

   --------------------
   -- Terrain_Chance --
   --------------------

   function Terrain_Chance
     (Climate : Root_Climate_Type'Class;
      Terrain : Hera.Terrain.Terrain_Type)
      return Unit_Real
   is
      use type Hera.Terrain.Terrain_Type;
   begin
      for Item of Climate.Terrain_Chance loop
         if Item.Terrain = Terrain then
            return Item.Chance;
         end if;
      end loop;
      return 0.0;
   end Terrain_Chance;

end Hera.Climate;
