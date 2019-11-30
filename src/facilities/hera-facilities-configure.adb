with Ada.Characters.Handling;

with WL.String_Maps;

with Hera.Quantities;

with Hera.Commodities.Configure;

package body Hera.Facilities.Configure is

   Facility_Version : constant Hera.Objects.Object_Version := "0.0.1";

   type Happiness_Rating is range 0 .. 16;

   function Happiness_Level
     (Service_Class : Hera.Commodities.Service_Class)
      return Happiness_Rating;

   type Facility_Creator is access
     function (Config : Tropos.Configuration)
               return Root_Facility_Type;

   package Creator_Maps is
     new WL.String_Maps (Facility_Creator);

   Creator_Map : Creator_Maps.Map;

   procedure Initialize_Creator_Map;

   function Create_Colony_Hub
     (Config : Tropos.Configuration)
      return Root_Facility_Type;

   function Create_Extractor
     (Config : Tropos.Configuration)
      return Root_Facility_Type;

   function Create_Factory
     (Config : Tropos.Configuration)
      return Root_Facility_Type;

   function Create_Orbital_Dock
     (Config : Tropos.Configuration)
      return Root_Facility_Type;

   function Create_Service
     (Config : Tropos.Configuration)
      return Root_Facility_Type;

   procedure Create_Modules
     (Facility       : in out Root_Facility_Type;
      Modules_Config : Tropos.Configuration);

   procedure Create_Employees
     (Facility        : in out Root_Facility_Type;
      Employees_Config : Tropos.Configuration);

   function Get_Capacity
     (Config : Tropos.Configuration)
      return Hera.Quantities.Quantity_Type
   is (Hera.Quantities.To_Quantity
         (Config.Get ("capacity", 0.0)));

   function Get_Power
     (Config : Tropos.Configuration)
      return Hera.Quantities.Quantity_Type
   is (Hera.Quantities.To_Quantity (Config.Get ("power", 0.0)));

   function Get_Service_Class
     (Config : Tropos.Configuration)
      return Hera.Commodities.Service_Class;

   procedure Deserialize_Common
     (Facility : in out Root_Facility_Type'Class;
      Config   : Tropos.Configuration);

   -----------------------
   -- Create_Colony_Hub --
   -----------------------

   function Create_Colony_Hub
     (Config : Tropos.Configuration)
      return Root_Facility_Type
   is
      Facility : Root_Facility_Type (Colony_Hub);
   begin
      Deserialize_Common (Facility, Config);
      return Facility;
   end Create_Colony_Hub;

   ----------------------
   -- Create_Employees --
   ----------------------

   procedure Create_Employees
     (Facility       : in out Root_Facility_Type;
      Employees_Config : Tropos.Configuration)
   is
   begin
      for Employee_Config of Employees_Config loop
         declare
            Pop_Class_Tag : constant String :=
              Employee_Config.Config_Name;
            Pop_Class    : constant Hera.Pops.Classes.Pop_Class_Type :=
              (if Hera.Pops.Classes.Exists (Pop_Class_Tag)
               then Hera.Pops.Classes.Get (Pop_Class_Tag)
               else raise Constraint_Error with
               Facility.Tag & ": no such employee type: "
               & Pop_Class_Tag);
            Quantity  : constant Hera.Quantities.Quantity_Type :=
              Hera.Quantities.To_Quantity (Employee_Config.Value);
         begin
            Facility.Employees.Append ((Pop_Class, Quantity));
         end;
      end loop;

   end Create_Employees;

   ----------------------
   -- Create_Extractor --
   ----------------------

   function Create_Extractor
     (Config : Tropos.Configuration)
      return Root_Facility_Type
   is
      use Ada.Characters.Handling;

      Facility : Root_Facility_Type (Extractor);

   begin

      Deserialize_Common (Facility, Config);

      for Resource in Facility.Resources'Range loop
         Facility.Resources (Resource) :=
           Config.Get (To_Lower (Resource'Image));
      end loop;

      return Facility;

   end Create_Extractor;

   ----------------------
   -- Create_Facility --
   ----------------------

   procedure Create_Facility
     (Facility_Config : Tropos.Configuration)
   is
      Class_Name : constant String :=
        Facility_Config.Get ("class", "no class field");

   begin
      if Creator_Map.Is_Empty then
         Initialize_Creator_Map;
      end if;

      if not Creator_Map.Contains (Class_Name) then
         raise Constraint_Error with
           "don't know how to create facility '"
           & Facility_Config.Config_Name
           & "'"
           & " with class '"
           & Class_Name
           & "'";
      end if;

      declare
         Facility : Root_Facility_Type :=
           Creator_Map.Element (Class_Name) (Facility_Config);
      begin
         Create_Modules
           (Facility, Facility_Config.Child ("module"));
         Create_Employees
           (Facility, Facility_Config.Child ("worker"));

         New_Facility
           (Facility => new Root_Facility_Type'(Facility));
      end;

   end Create_Facility;

   --------------------
   -- Create_Factory --
   --------------------

   function Create_Factory
     (Config : Tropos.Configuration)
      return Root_Facility_Type
   is
      use all type Hera.Commodities.Commodity_Class;

      Facility : Root_Facility_Type (Factory);

   begin

      Deserialize_Common (Facility, Config);

      Facility.Production (Consumer_Good) := Config.Get ("consumer");
      Facility.Production (Industrial_Good) := Config.Get ("industrial");
      Facility.Production (Building_Module) := Config.Get ("construction");
      Facility.Production (Starship_Component) := Config.Get ("shipyard");

      return Facility;

   end Create_Factory;

   --------------------
   -- Create_Modules --
   --------------------

   procedure Create_Modules
     (Facility       : in out Root_Facility_Type;
      Modules_Config : Tropos.Configuration)
   is
   begin
      for Module_Config of Modules_Config loop
         declare
            Module    : constant Hera.Commodities.Commodity_Type :=
              Hera.Commodities.Get (Module_Config.Config_Name);
            Quantity  : constant Hera.Quantities.Quantity_Type :=
              Hera.Quantities.To_Quantity (Module_Config.Value);
         begin
            Facility.Modules.Append ((Module, Quantity));
         end;
      end loop;

   end Create_Modules;

   -------------------------
   -- Create_Orbital_Dock --
   -------------------------

   function Create_Orbital_Dock
     (Config : Tropos.Configuration)
      return Root_Facility_Type
   is
      Facility : Root_Facility_Type (Orbital_Dock);
   begin
      Deserialize_Common (Facility, Config);
      return Facility;
   end Create_Orbital_Dock;

   --------------------
   -- Create_Service --
   --------------------

   function Create_Service
     (Config : Tropos.Configuration)
      return Root_Facility_Type
   is
      Service_Name : constant String := Config.Config_Name;
      Commodity     : constant Hera.Commodities.Commodity_Type :=
        Hera.Commodities.Get (Service_Name);
      Facility    : Root_Facility_Type (Service);
   begin
      Deserialize_Common (Facility, Config);
      Facility.Service := Commodity;
      Facility.Capacity :=
        Hera.Quantities.To_Quantity
          (case Commodity.Quality is
              when 1 => 1000.0,
              when 2 => 3000.0,
              when 3 => 7000.0);
      return Facility;
   end Create_Service;

   ---------------------
   -- Create_Services --
   ---------------------

   procedure Create_Services
     (Facility_Config : Tropos.Configuration)
   is
      Class_Name : constant String :=
        Facility_Config.Get ("class", "no class field");

   begin
      if Creator_Map.Is_Empty then
         Initialize_Creator_Map;
      end if;

      if not Creator_Map.Contains (Class_Name) then
         raise Constraint_Error with
           "don't know how to create facility '"
           & Facility_Config.Config_Name
           & "'"
           & " with class '"
           & Class_Name
           & "'";
      end if;

      if Class_Name = "service" then
         declare
            Service_Class : constant Hera.Commodities.Service_Class :=
              Get_Service_Class (Facility_Config);
            Service_Name  : constant String := Facility_Config.Config_Name;
         begin
            if not Hera.Commodities.Exists (Service_Name) then
               declare
                  Happiness     : constant Happiness_Rating :=
                    Happiness_Level (Service_Class);
                  Quality       : constant Quality_Type :=
                    Quality_Type (Positive'(Facility_Config.Get ("quality")));

                  Commodity     : constant Hera.Commodities.Commodity_Type :=
                    Hera.Commodities.Configure.New_Service_Commodity
                      (Tag       => Service_Name,
                       Charge    =>
                         Hera.Money.To_Price (100.0 * Real (Quality)),
                       Quality   => Quality,
                       Happiness =>
                         Real (Happiness) / Real (Happiness_Rating'Last),
                       Class     => Service_Class);
               begin
                  Commodity.Log ("new service commodity");
--                    pragma Unreferenced (Commodity);
               end;
            end if;
         end;
      end if;
   end Create_Services;

   ------------------------
   -- Deserialize_Common --
   ------------------------

   procedure Deserialize_Common
     (Facility : in out Root_Facility_Type'Class;
      Config   : Tropos.Configuration)
   is
   begin
      Facility.Initialize (Facility_Version, Config.Config_Name);
      Facility.Power := Get_Power (Config);
      Facility.Capacity := Get_Capacity (Config);
   end Deserialize_Common;

   -----------------------
   -- Get_Service_Class --
   -----------------------

   function Get_Service_Class
     (Config : Tropos.Configuration)
      return Hera.Commodities.Service_Class
   is
   begin
      return (if Config.Get ("medical")
              then Hera.Commodities.Medical
              elsif Config.Get ("fitness")
              then Hera.Commodities.Fitness
              elsif Config.Get ("entertainment")
              then Hera.Commodities.Entertainment
              elsif Config.Get ("education")
              then Hera.Commodities.Education
              else raise Constraint_Error with
                "no service type found in configuration for "
              & Config.Config_Name);
   end Get_Service_Class;

   ---------------------
   -- Happiness_Level --
   ---------------------

   function Happiness_Level
     (Service_Class : Hera.Commodities.Service_Class)
      return Happiness_Rating
   is
      use all type Hera.Commodities.Service_Class;
   begin
      return (case Service_Class is
                 when Medical       => 3,
                 when Fitness       => 1,
                 when Entertainment => 3,
                 when Education     => 2);
   end Happiness_Level;

   ----------------------------
   -- Initialize_Creator_Map --
   ----------------------------

   procedure Initialize_Creator_Map is

      procedure Add
        (Name : String;
         Fn   : Facility_Creator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name : String;
         Fn   : Facility_Creator)
      is
      begin
         Creator_Map.Insert (Name, Fn);
      end Add;

   begin
      Add ("colony-hub", Create_Colony_Hub'Access);
      Add ("extractor", Create_Extractor'Access);
      Add ("factory", Create_Factory'Access);
      Add ("orbital-dock", Create_Orbital_Dock'Access);
      Add ("service", Create_Service'Access);
   end Initialize_Creator_Map;

end Hera.Facilities.Configure;
