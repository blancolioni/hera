with Ada.Characters.Handling;

with WL.String_Maps;

with Hera.Elementary_Functions;

package body Hera.Commodities.Configure is

   Commodity_Version : constant Hera.Objects.Object_Version := "0.0.1";

   type Happiness_Rating is range 0 .. 16;

   function Happiness_Level
     (Class : Consumer_Class)
      return Happiness_Rating
   is (case Class is
          when Food => 1,
          when Drink => 1,
          when Intoxicant => 3,
          when Clothing   => 2);

   type Commodity_Creator is access
     function (Config : Tropos.Configuration)
               return Root_Commodity_Type;

   package Creator_Maps is
     new WL.String_Maps (Commodity_Creator);

   type Commodity_Access is access all Root_Commodity_Type'Class;

   package Commodity_Maps is
     new WL.String_Maps (Commodity_Access);

   Current : Commodity_Maps.Map;

   Creator_Map : Creator_Maps.Map;

   procedure Initialize_Creator_Map;

   function Create_Building_Module
     (Config : Tropos.Configuration)
      return Root_Commodity_Type;

   function Create_Consumer_Good
     (Config : Tropos.Configuration)
      return Root_Commodity_Type;

   function Create_Industrial_Good
     (Config : Tropos.Configuration)
      return Root_Commodity_Type;

   function Create_Resource
     (Config : Tropos.Configuration)
      return Root_Commodity_Type;

   function Create_Starship_Part
     (Config : Tropos.Configuration)
      return Root_Commodity_Type;

   function Get_Price
     (Config : Tropos.Configuration)
      return Hera.Money.Price_Type
   is (Hera.Money.To_Price (Config.Get ("npc-price")));

   function Get_Mass
     (Config : Tropos.Configuration)
      return Non_Negative_Real
   is (Config.Get ("mass"));

--     function Get_Version
--       (Config : Tropos.Configuration)
--        return Hera.Objects.Object_Version
--     is (Hera.Objects.Object_Version
--         (String'(Config.Get ("version", "0.0.1"))));

   procedure Deserialize_Common
     (Commodity : in out Root_Commodity_Type'Class;
      Config : Tropos.Configuration);

   ----------------------------
   -- Create_Building_Module --
   ----------------------------

   function Create_Building_Module
     (Config : Tropos.Configuration)
      return Root_Commodity_Type
   is
      use Ada.Characters.Handling;
      Module : Root_Commodity_Type (Building_Module);

   begin
      Deserialize_Common (Module, Config);

      Module.Building_Module_Type := Structural;
      for Class in Building_Module_Class loop
         if Config.Contains (To_Lower (Class'Image)) then
            Module.Building_Module_Type := Class;
            exit;
         end if;
      end loop;

      return Module;

   end Create_Building_Module;

   ----------------------
   -- Create_Commodity --
   ----------------------

   procedure Create_Commodity
     (Config : Tropos.Configuration)
   is
      Class_Name : constant String :=
        Config.Get ("class", "no class field");

   begin
      if Creator_Map.Is_Empty then
         Initialize_Creator_Map;
      end if;

      if not Creator_Map.Contains (Class_Name) then
         raise Constraint_Error with
           "don't know how to create commodity '"
           & Config.Config_Name
           & "'"
           & " with class '"
           & Class_Name
           & "'";
      end if;

      declare
         Commodity : constant Root_Commodity_Type :=
           Creator_Map.Element (Class_Name) (Config);
      begin
         Current.Insert
           (Commodity.Tag, new Root_Commodity_Type'(Commodity));
      end;

   end Create_Commodity;

   -----------------------
   -- Create_Components --
   -----------------------

   procedure Create_Components
     (Config : Tropos.Configuration)
   is
      Tag              : constant String := Config.Config_Name;
      Commodity        : constant Commodity_Access :=
        Current.Element (Tag);
      Component_Count : Natural := 0;
   begin
      for Component_Config of Config.Child ("component") loop
         declare
            Component : constant Commodity_Access :=
              Current.Element (Component_Config.Config_Name);
            Quantity  : constant Hera.Quantities.Quantity_Type :=
              Hera.Quantities.To_Quantity (Component_Config.Value);
         begin
            if Component = null then
               raise Constraint_Error with
                 "component " & Component_Config.Config_Name
                 & " not found in configuration for "
                 & Config.Config_Name;
            end if;

            Commodity.Components.Append
              ((Commodity_Type (Component), Quantity));

            Component_Count := Component_Count + 1;
         end;
      end loop;

      declare
         use Hera.Money;
      begin
         if Commodity.Components.Is_Empty
           and then Commodity.Price = Zero
         then
            raise Constraint_Error with
              "commodity '" & Config.Config_Name
              & "' has neither price nor components";
         end if;
      end;

   end Create_Components;

   --------------------------
   -- Create_Consumer_Good --
   --------------------------

   function Create_Consumer_Good
     (Config : Tropos.Configuration)
      return Root_Commodity_Type
   is
      Class      : constant Consumer_Class :=
        (if Config.Get ("food")
         then Food
         elsif Config.Get ("drink")
         then Drink
         elsif Config.Get ("intoxicant")
         then Intoxicant
         elsif Config.Get ("clothes")
         then Clothing
         else raise Constraint_Error with
           "cannot determine class of consumer good: " & Config.Config_Name);

      Commodity : Root_Commodity_Type (Consumer_Good);

   begin
      Deserialize_Common (Commodity, Config);
      Commodity.Happiness :=
        Real (Happiness_Level (Class))
          / Real (Happiness_Rating'Last);
      Commodity.Consumer_Type := Class;
      Commodity.Consumer_Quality :=
        Quality_Type (Positive'(Config.Get ("quality")));

      return Commodity;
   end Create_Consumer_Good;

   ----------------------------
   -- Create_Industrial_Good --
   ----------------------------

   function Create_Industrial_Good
     (Config : Tropos.Configuration)
      return Root_Commodity_Type
   is
      Class      : constant Industrial_Class :=
        (if Config.Get ("alloy")
         then Alloy
         elsif Config.Get ("ceramic")
         then Ceramic
         elsif Config.Get ("electronic")
         then Electronic
         elsif Config.Get ("plastic")
         then Plastic
         elsif Config.Get ("power")
         then Power
         elsif Config.Get ("engine")
         then Engine
         else raise Constraint_Error with
           "cannot determine class of industrial good: " & Config.Config_Name);

      Commodity  : Root_Commodity_Type (Industrial_Good);

   begin
      Deserialize_Common (Commodity, Config);

      Commodity.Industrial_Type := Class;

      return Commodity;
   end Create_Industrial_Good;

   -------------------
   -- Create_Prices --
   -------------------

   procedure Create_Prices is
      use Hera.Money;
      Finished : Boolean := False;
   begin
      while not Finished loop
         Finished := True;

         for Item of Current loop
            if Item.Price = Zero then
               declare
                  Complete : Boolean := True;
                  Cost     : Money_Type := Zero;
               begin
                  for Component of Item.Components loop
                     if Component.Commodity.Price > Zero then
                           Cost := Cost
                          + Total (Component.Commodity.Price,
                                   Component.Quantity);
                     else
                        Complete := False;
                     end if;
                  end loop;

                  if Complete then
                     Cost := Adjust (Cost, 1.5);
                     declare
                        X : Non_Negative_Real := To_Real (Cost);
                        D : constant Integer :=
                          Integer
                            (Hera.Elementary_Functions.Log (X, 10.0)) - 1;
                        R : constant Non_Negative_Real := 10.0 ** D;
                     begin
                        X := (Real'Floor (X / R) + 1.0) * R;

                        Item.Price := To_Price (X);
                     end;
                  else
                     Finished := False;
                  end if;
               end;
            end if;
         end loop;
      end loop;

      for Commodity of Current loop
         New_Commodity (Commodity_Type (Commodity));
      end loop;

   end Create_Prices;

   ---------------------
   -- Create_Resource --
   ---------------------

   function Create_Resource
     (Config : Tropos.Configuration)
      return Root_Commodity_Type
   is
      Class      : constant Resource_Class :=
        (if Config.Get ("organic")
         then Organic
         elsif Config.Get ("mineral")
         then Mineral
         elsif Config.Get ("metal")
         then Metal
         elsif Config.Get ("fissile")
         then Fissile
         elsif Config.Get ("fuel")
         then Fuel
         elsif Config.Get ("gas")
         then Gas
         elsif Config.Get ("liquid")
         then Liquid
         else raise Constraint_Error with
           "cannot determine class of resource: " & Config.Config_Name);

      Commodity  : Root_Commodity_Type (Resource);

   begin
      Deserialize_Common (Commodity, Config);

      Commodity.Resource_Type := Class;

      return Commodity;
   end Create_Resource;

   --------------------------
   -- Create_Starship_Part --
   --------------------------

   function Create_Starship_Part
     (Config : Tropos.Configuration)
      return Root_Commodity_Type
   is

      function Get (Field_Name : String) return Hera.Quantities.Quantity_Type
      is (Hera.Quantities.To_Quantity (Config.Get (Field_Name, 0.0)));

      Part : constant Starship_Component_Type :=
        Starship_Component_Type'
          (Consumer_Capacity   => Get ("consumer-capacity"),
           Industrial_Capacity => Get ("industrial-capacity"),
           Module_Capacity     => Get ("modules-capacity"),
           Organic_Capacity    => Get ("organics-capacity"),
           Liquid_Gas_Capacity => Get ("liquidgas-capacity"),
           Thrust              => Config.Get ("thrust", 0),
           Warp                => Config.Get ("warp", 0),
           Long_Range_Scan     => Config.Get ("long-scan", 0),
           Surface_Scan        => Config.Get ("short-scan"),
           Asteroid_Mining     => Config.Get ("asteroid"),
           Gas_Field_Ming      => Config.Get ("gasfield"),
           Command             => Config.Get ("command"),
           Crew                => Config.Get ("crew"),
           System_Probe        => Config.Get ("system-probe"),
           Planet_Probe        => Config.Get ("planet-probe"),
           Bio_Lab             => Config.Get ("bio-lab"),
           Physics_Lab         => Config.Get ("physics-lab"),
           Geo_Lab             => Config.Get ("geo-lab"));

      Commodity  : Root_Commodity_Type (Starship_Component);

   begin
      Deserialize_Common (Commodity, Config);

      Commodity.Component := Part;

      return Commodity;
   end Create_Starship_Part;

   ------------------------
   -- Deserialize_Common --
   ------------------------

   procedure Deserialize_Common
     (Commodity : in out Root_Commodity_Type'Class;
      Config    : Tropos.Configuration)
   is
   begin
      Commodity.Initialize (Commodity_Version, Config.Config_Name);
      Commodity.Mass := Get_Mass (Config);
      Commodity.Price := Get_Price (Config);
      Commodity.Transient := Config.Get ("transient");
   end Deserialize_Common;

   ----------------------------
   -- Initialize_Creator_Map --
   ----------------------------

   procedure Initialize_Creator_Map is

      procedure Add
        (Name : String;
         Fn   : Commodity_Creator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name : String;
         Fn   : Commodity_Creator)
      is
      begin
         Creator_Map.Insert (Name, Fn);
      end Add;

   begin
      Add ("building-module", Create_Building_Module'Access);
      Add ("consumer-good", Create_Consumer_Good'Access);
      Add ("industrial-good", Create_Industrial_Good'Access);
      Add ("starship-part", Create_Starship_Part'Access);
      Add ("resource", Create_Resource'Access);
   end Initialize_Creator_Map;

   ---------------------------
   -- New_Service_Commodity --
   ---------------------------

   function New_Service_Commodity
     (Tag       : String;
      Charge    : Hera.Money.Price_Type;
      Quality   : Quality_Type;
      Happiness : Unit_Real;
      Class     : Service_Class)
      return Commodity_Type
   is
      Commodity : Root_Commodity_Type (Service_Token);
   begin
      Commodity.Initialize (Commodity_Version, Tag);
      Commodity.Mass := 0.0;
      Commodity.Price := Charge;
      Commodity.Transient := True;
      Commodity.Service_Type := Class;
      Commodity.Service_Quality := Quality;
      Commodity.Happiness := Happiness;
      return Result : constant Commodity_Type :=
        new Root_Commodity_Type'(Commodity)
      do
         New_Commodity (Result);
      end return;
   end New_Service_Commodity;

end Hera.Commodities.Configure;
