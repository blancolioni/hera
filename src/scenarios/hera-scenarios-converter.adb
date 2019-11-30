with WL.String_Maps;

with Tropos.Properties_Reader;
with Tropos.Writer;

with Hera.Real_Images;

package body Hera.Scenarios.Converter is

   package Class_Converter_Maps is
     new WL.String_Maps (String);

   Class_Converter : Class_Converter_Maps.Map;

   function Convert_Item
     (Item_Config : Tropos.Configuration)
      return Tropos.Configuration;

   function Convert_Facility
     (Facility_Config : Tropos.Configuration)
      return Tropos.Configuration;

   procedure Convert_Structure
     (Structure_Config : Tropos.Configuration;
      Target_Config    : in out Tropos.Configuration);

   procedure Convert_Quantity_List
     (Quantity_Config : Tropos.Configuration;
      Target_Config   : in out Tropos.Configuration);

   procedure Convert_Chance_List
     (Chance_Config : Tropos.Configuration;
      Target_Config : in out Tropos.Configuration);

   function Normalize (Text : String) return String
   is (if Text = "vegatables"
       then "vegetables"
       elsif Text = "drone"
       then "worker"
       elsif Text = "hub"
       then "colony-hub"
       elsif Text = "dock"
       then "orbital-dock"
       else Text);

   -------------------------
   -- Convert_Chance_List --
   -------------------------

   procedure Convert_Chance_List
     (Chance_Config : Tropos.Configuration;
      Target_Config : in out Tropos.Configuration)
   is
      Components   : Tropos.Configuration :=
        Tropos.New_Config (Chance_Config.Config_Name);
      Types  : constant Tropos.Configuration :=
        Chance_Config.Child ("type");
      Chances      : constant Tropos.Configuration :=
        Chance_Config.Child ("chance");
   begin
      for I in 1 .. Types.Child_Count loop
         Components.Add
           (Normalize (Types.Child (I).Value),
            Hera.Real_Images.Approximate_Image
              (Real'(Chances.Child (I).Value) / 100.0));
      end loop;
      Target_Config.Add (Components);
   end Convert_Chance_List;

   ------------------------
   -- Convert_Facilities --
   ------------------------

   procedure Convert_Facilities
     (Properties_Path : String;
      Config_Path     : String)
   is
      Properties_Config : constant Tropos.Configuration :=
        Tropos.Properties_Reader.Read_Properties
          (Properties_Path);
   begin
      Class_Converter.Insert
        ("starcorp.common.types.ColonyHub", "colony-hub");
      Class_Converter.Insert
        ("starcorp.common.types.OrbitalDock", "orbital-dock");
      Class_Converter.Insert
        ("starcorp.common.types.Factory", "factory");
      Class_Converter.Insert
        ("starcorp.common.types.ResourceGenerator", "extractor");
      Class_Converter.Insert
        ("starcorp.common.types.ServiceFacility", "service");

      for Facility_Config of Properties_Config loop
         declare
            Config : constant Tropos.Configuration :=
              Convert_Facility (Facility_Config);
         begin
            Tropos.Writer.Write_Config
              (Config => Config,
               Path   =>
                 Config_Path
               & "/" & Config.Config_Name
               & ".facility");
         end;
      end loop;
   end Convert_Facilities;

   ----------------------
   -- Convert_Facility --
   ----------------------

   function Convert_Facility
     (Facility_Config : Tropos.Configuration)
      return Tropos.Configuration
   is
   begin
      return Config : Tropos.Configuration :=
        Tropos.New_Config (Normalize (Facility_Config.Config_Name))
      do
         declare
            Facility_Class : constant String := Facility_Config.Get ("class");
         begin
            if not Class_Converter.Contains (Facility_Class) then
               raise Constraint_Error with
                 "unrecognised class '" & Facility_Class
                 & "' in configuration for "
                 & Facility_Config.Config_Name;
            end if;

            Config.Add ("class", Class_Converter.Element (Facility_Class));

            for Field_Config of Facility_Config loop
               declare
                  Field_Name : constant String := Field_Config.Config_Name;
               begin
                  if Field_Name = "class" then
                     null;
                  elsif Field_Name = "module"
                    or else Field_Name = "worker"
                  then
                     Convert_Quantity_List (Field_Config, Config);
                  elsif Field_Name = "lab"
                  then
                     Convert_Structure (Field_Config, Config);
                  elsif Field_Config.Child_Count = 0 then
                     raise Constraint_Error with
                       "orphaned field '" & Field_Name & "'"
                       & " in configuration for "
                       & Facility_Config.Config_Name;
                  elsif Field_Config.Child_Count > 1 then
                     raise Constraint_Error with
                       "unrecognised complex field '" & Field_Name & "'"
                       & " in configuration for "
                       & Facility_Config.Config_Name;
                  else
                     Config.Add (Field_Name, String'(Field_Config.Value));
                  end if;
               end;
            end loop;
         end;
      end return;
   end Convert_Facility;

   ------------------
   -- Convert_Item --
   ------------------

   function Convert_Item
     (Item_Config : Tropos.Configuration)
      return Tropos.Configuration
   is
   begin
      return Config : Tropos.Configuration :=
        Tropos.New_Config (Normalize (Item_Config.Config_Name))
      do
         declare
            Item_Class : constant String := Item_Config.Get ("class");
         begin
            if not Class_Converter.Contains (Item_Class) then
               raise Constraint_Error with
                 "unrecognised class '" & Item_Class
                 & "' in configuration for "
                 & Item_Config.Config_Name;
            end if;

            Config.Add ("class", Class_Converter.Element (Item_Class));

            for Field_Config of Item_Config loop
               declare
                  Field_Name : constant String := Field_Config.Config_Name;
               begin
                  if Field_Name = "class" then
                     null;
                  elsif Field_Name = "component" then
                     Convert_Quantity_List (Field_Config, Config);
                  elsif Field_Name = "capacity"
                    or else Field_Name = "lab"
                    or else Field_Name = "probe"
                    or else Field_Name = "scan"
                  then
                     Convert_Structure (Field_Config, Config);
                  elsif Field_Config.Child_Count = 0 then
                     raise Constraint_Error with
                       "orphaned field '" & Field_Name & "'"
                       & " in configuration for "
                       & Item_Config.Config_Name;
                  elsif Field_Config.Child_Count > 1 then
                     raise Constraint_Error with
                       "unrecognised complex field '" & Field_Name & "'"
                       & " in configuration for "
                       & Item_Config.Config_Name;
                  else
                     Config.Add (Field_Name, String'(Field_Config.Value));
                  end if;
               end;
            end loop;
         end;
      end return;
   end Convert_Item;

   ----------------------------------
   -- Convert_Items_To_Commodities --
   ----------------------------------

   procedure Convert_Items_To_Commodities
     (Items_Path       : String;
      Commodities_Path : String)
   is
      Items_Config : constant Tropos.Configuration :=
        Tropos.Properties_Reader.Read_Properties
          (Items_Path);
   begin
      Class_Converter.Insert
        ("starcorp.common.types.Resources", "resource");
      Class_Converter.Insert
        ("starcorp.common.types.ConsumerGoods", "consumer-good");
      Class_Converter.Insert
        ("starcorp.common.types.IndustrialGoods", "industrial-good");
      Class_Converter.Insert
        ("starcorp.common.types.BuildingModules", "building-module");
      Class_Converter.Insert
        ("starcorp.common.types.StarshipHulls", "starship-part");

      for Item_Config of Items_Config loop
         declare
            Config : constant Tropos.Configuration :=
              Convert_Item (Item_Config);
         begin
            Tropos.Writer.Write_Config
              (Config => Config,
               Path   =>
                 Commodities_Path
               & "/" & Config.Config_Name
               & ".commodity");
         end;
      end loop;
   end Convert_Items_To_Commodities;

   -------------------------
   -- Convert_Pop_Classes --
   -------------------------

   procedure Convert_Pop_Classes
     (Properties_Path : String;
      Config_Path     : String)
   is
      Properties_Config : constant Tropos.Configuration :=
        Tropos.Properties_Reader.Read_Properties
          (Properties_Path);
   begin
      for Pop_Class_Config of Properties_Config loop
         declare
            Config : Tropos.Configuration :=
              Tropos.New_Config
                (Normalize (Pop_Class_Config.Config_Name));
         begin
            Convert_Structure (Pop_Class_Config.Child ("quality"),
                               Config);
            Config.Add ("salary",
                        String'(Pop_Class_Config.Get ("salary")));

            Tropos.Writer.Write_Config
              (Config => Config,
               Path   =>
                 Config_Path
               & "/" & Config.Config_Name
               & ".pop");
         end;
      end loop;
   end Convert_Pop_Classes;

   ---------------------------
   -- Convert_Quantity_List --
   ---------------------------

   procedure Convert_Quantity_List
     (Quantity_Config : Tropos.Configuration;
      Target_Config    : in out Tropos.Configuration)
   is
      Components   : Tropos.Configuration :=
        Tropos.New_Config (Quantity_Config.Config_Name);
      Type_Config  : constant Tropos.Configuration :=
        Quantity_Config.Child ("type");
      Qty_Config   : constant Tropos.Configuration :=
        Quantity_Config.Child ("qty");
   begin
      for I in 1 .. Type_Config.Child_Count loop
         Components.Add (Normalize (Type_Config.Child (I).Value),
                         Natural'(Qty_Config.Child (I).Value));
      end loop;
      Target_Config.Add (Components);
   end Convert_Quantity_List;

   -----------------------
   -- Convert_Structure --
   -----------------------

   procedure Convert_Structure
     (Structure_Config : Tropos.Configuration;
      Target_Config    : in out Tropos.Configuration)
   is
   begin
      for Child of Structure_Config loop
         Target_Config.Add
           (Normalize (Child.Config_Name)
            & "-"
            & Normalize (Structure_Config.Config_Name),
            String'(Child.Value));
      end loop;
   end Convert_Structure;

   ---------------------
   -- Convert_Terrain --
   ---------------------

   procedure Convert_Terrain
     (Properties_Path : String;
      Config_Path     : String)
   is
      Properties_Config : constant Tropos.Configuration :=
        Tropos.Properties_Reader.Read_Properties
          (Properties_Path);
      Next_Priority : Natural := 0;
   begin
      for Terrain_Config of Properties_Config loop
         declare
            Config : Tropos.Configuration :=
              Tropos.New_Config (Normalize (Terrain_Config.Config_Name));
         begin
            Next_Priority := Next_Priority + 1;
            Config.Add ("hazard", String'(Terrain_Config.Get ("hazard")));
            Config.Add ("priority", Next_Priority);
            Convert_Chance_List
              (Terrain_Config.Child ("resource"), Config);

            Tropos.Writer.Write_Config
              (Config => Config,
               Path   =>
                 Config_Path
               & "/" & Config.Config_Name
               & ".terrain");
         end;
      end loop;
   end Convert_Terrain;

end Hera.Scenarios.Converter;
