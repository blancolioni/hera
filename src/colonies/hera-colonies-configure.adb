with Ada.Containers.Doubly_Linked_Lists;

with WL.Random;
with WL.String_Maps;

with Hera.Facilities;

with Hera.Names;

with Hera.Commodities;
with Hera.Installations.Configure;
with Hera.Markets;

package body Hera.Colonies.Configure is

   type Random_Calculation is
      record
         Low, High  : Natural;
         Factor     : Natural;
         Plus       : Natural;
      end record;

   function Evaluate (Calculation : Random_Calculation) return Natural
   is (WL.Random.Random_Number (Calculation.Low, Calculation.High)
       * Calculation.Factor + Calculation.Plus);

   function To_Random_Calculation
     (Config : Tropos.Configuration)
      return Random_Calculation;

   type Initial_Facility is
      record
         Facility : Hera.Facilities.Facility_Type;
         Count    : Random_Calculation;
      end record;

   package Initial_Facility_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Initial_Facility);

   type Colony_Template is
      record
         Extractors  : Random_Calculation;
         Facilities  : Initial_Facility_Lists.List;
         Market_Size : Natural;
      end record;

   package Colony_Template_Maps is
     new WL.String_Maps (Colony_Template);

   Colony_Template_Map : Colony_Template_Maps.Map;

   function Pick_Corporation
     (Corporations : Hera.Corporations.Corporation_Array)
      return Hera.Corporations.Corporation_Type
   is (Corporations
       (WL.Random.Random_Number
        (Corporations'First,
         Corporations'Last)));

   ------------------------
   -- Configure_Template --
   ------------------------

   procedure Configure_Template
     (Config : Tropos.Configuration)
   is
      Template : Colony_Template :=
        Colony_Template'
          (Extractors  => To_Random_Calculation (Config.Child ("extractors")),
           Facilities  => <>,
           Market_Size => Config.Get ("market-size"));
   begin
      for Item_Config of Config loop
         declare
            Field : constant String := Item_Config.Config_Name;
         begin
            if Field = "extractors"
              or else Field = "market-size"
            then
               null;
            elsif not Hera.Facilities.Exists (Field) then
               raise Constraint_Error with
                 "in colony template '"
                 & Config.Config_Name
                 & "': no such facility: "
                 & Field;
            else
               declare
                  Facility : constant Hera.Facilities.Facility_Type :=
                               Hera.Facilities.Get (Field);
               begin
                  Template.Facilities.Append
                    (Initial_Facility'
                       (Facility => Facility,
                        Count    => To_Random_Calculation (Item_Config)));
               end;
            end if;
         end;
      end loop;

      Colony_Template_Map.Insert
        (Config.Config_Name, Template);

   end Configure_Template;

   ------------
   -- Create --
   ------------

   procedure Create
     (Planet        : Hera.Planets.Planet_Type;
      Sector        : Hera.Sectors.Sector_Type;
      Corporations  : Hera.Corporations.Corporation_Array;
      Template_Name : String)
   is
      use Hera.Quantities;

      type Population_Record is
         record
            Class    : Hera.Pops.Classes.Pop_Class_Type;
            Quantity : Quantity_Type;
         end record;

      package Population_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Population_Record);

      Initial_Pops : Population_Lists.List;

      function Create_Warehouses return Warehouse_Lists.List;

      procedure Add_Pop
        (To       : in out Population_Lists.List;
         Class    : Hera.Pops.Classes.Pop_Class_Type;
         Quantity : Hera.Quantities.Quantity_Type);

      -------------
      -- Add_Pop --
      -------------

      procedure Add_Pop
        (To       : in out Population_Lists.List;
         Class    : Hera.Pops.Classes.Pop_Class_Type;
         Quantity : Hera.Quantities.Quantity_Type)
      is
         use type Hera.Pops.Classes.Pop_Class_Type;
      begin
         for Item of To loop
            if Item.Class = Class then
               Item.Quantity := Item.Quantity + Quantity;
               return;
            end if;
         end loop;
         To.Append ((Class, Quantity));
      end Add_Pop;

      -----------------------
      -- Create_Warehouses --
      -----------------------

      function Create_Warehouses return Warehouse_Lists.List is
      begin
         return Ws : Warehouse_Lists.List do
            for Corp of Corporations loop
               Ws.Append
                 (Hera.Warehouses.New_Warehouse (Corp));
            end loop;
         end return;
      end Create_Warehouses;

   begin
      if not Colony_Template_Map.Contains (Template_Name) then
         raise Constraint_Error with
           "no such template for colony on " & Planet.Name & ": "
           & Template_Name;
      end if;

      declare
         Template : constant Colony_Template :=
           Colony_Template_Map.Element (Template_Name);
         Government : constant Hera.Corporations.Corporation_Type :=
                        Pick_Corporation (Corporations);
         Market     : constant Hera.Markets.Market_Type :=
           Hera.Markets.Create_Market;
         Colony_Record : constant Root_Colony_Type :=
                           Root_Colony_Type'
             (Hera.Objects.Root_Named_Object with
              Index => Next_Index,
              Government    => Government,
              Planet        => Planet,
              Sector        => Sector,
              Hazard_Level  =>
                Sector.Terrain.Hazard_Level
              + (1.0 - Planet.Habitability),
              Founded       => Hera.Calendar.Clock,
              Market_Size   => Template.Market_Size,
              Market        => Market,
              Pops          => Pop_Lists.Empty_List,
              Warehouses    => Create_Warehouses);

         Colony : constant Colony_Type :=
                    New_Colony (Colony_Record, Hera.Names.Random_Colony_Name);

         procedure Build
           (Facility : Hera.Facilities.Facility_Type;
            Pops     : in out Population_Lists.List);

         procedure Build_Resource_Extractors
           (Sector : Hera.Sectors.Sector_Type;
            Pops     : in out Population_Lists.List;
            Count    : Natural);

         -----------
         -- Build --
         -----------

         procedure Build
           (Facility : Hera.Facilities.Facility_Type;
            Pops     : in out Population_Lists.List)
         is
            use all type Hera.Facilities.Facility_Class;
            Owner : constant Hera.Corporations.Corporation_Type :=
                      (if Facility.Class = Colony_Hub
                       then Government
                       else Pick_Corporation (Corporations));
         begin
            Hera.Installations.Configure.Initial_Installation
              (Owner    => Owner,
               Colony   => Colony,
               Facility => Facility);

            for Worker of Facility.Workers loop
               Add_Pop (Pops, Worker, Facility.Quantity (Worker));
            end loop;

         end Build;

         -------------------------------
         -- Build_Resource_Extractors --
         -------------------------------

         procedure Build_Resource_Extractors
           (Sector : Hera.Sectors.Sector_Type;
            Pops     : in out Population_Lists.List;
            Count    : Natural)
         is
            Max_Facilities : constant := 16;
            Facility_Count : Natural := 0;
            Facility_List  : array (1 .. Max_Facilities)
              of Hera.Facilities.Facility_Type;

         begin
            for Resource of Sector.Resources loop
               for Extractor of
                 Hera.Facilities.Get_Extractors (Resource)
               loop
                  Facility_Count := Facility_Count + 1;
                  Facility_List (Facility_Count) := Extractor;
               end loop;
            end loop;

            if Facility_Count > 0 then
               declare
                  Index : Natural := 0;
               begin
                  for I in 1 .. Count loop
                     Index := Index + 1;
                     if Index > Facility_Count then
                        Index := 1;
                     end if;

                     declare
                        Facility : constant Hera.Facilities.Facility_Type :=
                          Facility_List (Index);
                     begin
                        Hera.Installations.Configure.Initial_Installation
                          (Owner    => Pick_Corporation (Corporations),
                           Colony   => Colony,
                           Facility => Facility);

                        for Worker of Facility.Workers loop
                           Add_Pop (Pops, Worker, Facility.Quantity (Worker));
                        end loop;
                     end;
                  end loop;
               end;
            end if;
         end Build_Resource_Extractors;

      begin

         for Item of Template.Facilities loop
            declare
               Count : constant Natural :=
                 Evaluate (Item.Count);
            begin
               for I in 1 .. Count loop
                  Build (Item.Facility, Initial_Pops);
               end loop;
            end;
         end loop;

         Build_Resource_Extractors
           (Sector, Initial_Pops,
            Evaluate (Template.Extractors));

         declare
            New_Pops : Population_Lists.List;
            Min_Pop  : constant array (Quality_Type) of Non_Negative_Real :=
              (1000.0, 200.0, 50.0);
         begin
            for Pop of Initial_Pops loop
               if Pop.Quantity >
                 To_Quantity (Min_Pop (Pop.Class.Service_Quality))
               then
                  for Facility of
                    Hera.Facilities.Get_Services
                      (Pop.Class.Service_Quality)
                  loop
                     declare
                        Size : constant Quantity_Type := Pop.Quantity;
                        Quality : constant Quality_Type :=
                          Pop.Class.Service_Quality;
                        Adjusted_Size : constant Non_Negative_Real :=
                          To_Real (Size) - Min_Pop (Quality);
                        Capacity : constant Non_Negative_Real :=
                          To_Real (Facility.Capacity);
                        Real_Count    : constant Non_Negative_Real :=
                          Adjusted_Size / Capacity;
                        Count : constant Natural :=
                          Natural (Real'Floor (Real_Count));
                     begin
                        for I in 1 .. Count loop
                           Build (Facility, New_Pops);
                        end loop;
                     end;
                  end loop;
               end if;
            end loop;

            for Pop of New_Pops loop
               Add_Pop (Initial_Pops, Pop.Class, Pop.Quantity);
            end loop;
         end;

         for Pop of Initial_Pops loop
            Colony.Log ("initial " & Pop.Class.Tag & " population: "
                        & Hera.Quantities.Show (Pop.Quantity));
            Colony.Add_Initial_Population
              (Pop.Class, Pop.Quantity);

            declare
               procedure Initial_Stock
                 (Commodity : Hera.Commodities.Commodity_Type);

               -------------------
               -- Initial_Stock --
               -------------------

               procedure Initial_Stock
                 (Commodity : Hera.Commodities.Commodity_Type)
               is
                  use all type Hera.Commodities.Commodity_Class;
                  Corporation : constant Hera.Corporations.Corporation_Type :=
                    Pick_Corporation (Corporations);
                  Quantity    : constant Quantity_Type :=
                    (if Commodity.Class = Service_Token
                     then Scale (Pop.Quantity, 2.0)
                     else Scale (Pop.Quantity, 5.0));
               begin
                  Colony.Log ("adding " & Show (Quantity) & " "
                              & Commodity.Tag
                              & " owned by " & Corporation.Name);
                  Colony.Add_Initial_Stock
                    (Owner       => Corporation,
                     Commodity   => Commodity,
                     Quantity    => Quantity);
               end Initial_Stock;

            begin
               Pop.Class.Iterate_Required_Goods
                 (Process => Initial_Stock'Access);
            end;
         end loop;

         declare
            Quantity : constant Quantity_Type :=
              To_Quantity (Real (Template.Market_Size));
         begin
            for Commodity of Hera.Commodities.Resources loop
               Colony.Add_Initial_Stock
                 (Owner       => Pick_Corporation (Corporations),
                  Commodity   => Commodity,
                  Quantity    => Quantity);
            end loop;
            for Commodity of Hera.Commodities.Industrial_Goods loop
               Colony.Add_Initial_Stock
                 (Owner       => Pick_Corporation (Corporations),
                  Commodity   => Commodity,
                  Quantity    => Quantity);
            end loop;
         end;

         if Template.Market_Size >= 1000 then
            declare
               Quantity : constant Quantity_Type :=
                 To_Quantity
                   (Real (Template.Market_Size) / 1000.0);
            begin
               for Commodity of Hera.Commodities.Building_Modules loop
                  Colony.Add_Initial_Stock
                    (Owner       => Pick_Corporation (Corporations),
                     Commodity   => Commodity,
                     Quantity    => Quantity);
               end loop;
            end;
         end if;

         if Template.Market_Size >= 10_000 then
            declare
               Quantity : constant Quantity_Type :=
                 To_Quantity (Real (Template.Market_Size)
                              / 10000.0);
            begin
               for Commodity of Hera.Commodities.Starship_Components loop
                  Colony.Add_Initial_Stock
                    (Owner       => Pick_Corporation (Corporations),
                     Commodity   => Commodity,
                     Quantity    => Quantity);
               end loop;
            end;
         end if;
      end;

   end Create;

   ---------------------------
   -- To_Random_Calculation --
   ---------------------------

   function To_Random_Calculation
     (Config : Tropos.Configuration)
      return Random_Calculation
   is
   begin
      if Config.Child_Count = 0 then
         return (0, 0, 0, 0);
      elsif Config.Child_Count = 1 then
         declare
            K : constant Natural := Config.Value;
         begin
            return (K, K, 1, 0);
         end;
      elsif Config.Child_Count = 2 then
         return (Config.Get (1), Config.Get (2), 1, 0);
      elsif Config.Child_Count = 3 then
         return (Config.Get (1), Config.Get (2), Config.Get (3), 0);
      elsif Config.Child_Count = 4 then
         return (Config.Get (1), Config.Get (2),
                 Config.Get (3), Config.Get (4));
      else
         raise Constraint_Error with
           "invalid random calculation";
      end if;
   end To_Random_Calculation;

end Hera.Colonies.Configure;
