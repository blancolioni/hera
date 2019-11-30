with Ada.Characters.Handling;

with WL.Numerics.Roman;

with Hera.Atmosphere.Configure;
with Hera.Calendar;
with Hera.Elementary_Functions;
with Hera.Objects;
with Hera.Random;
with Hera.Solar_System;

package body Hera.Planets.Configure is

   Planet_Version : constant Hera.Objects.Object_Version := "0.0.1";

   function To_Climate_Type
     (Climate : Planet_Climate)
      return Hera.Climate.Climate_Type
   is (Hera.Climate.Get
       (Ada.Characters.Handling.To_Lower
          (Climate'Image)));

   package Planet_Tables is

      function Random_Planet_Mass
        (Zone : Planetary_Zone)
         return Non_Negative_Real;

      function Composition
        (Mass : Non_Negative_Real;
         Zone : Orbit_Zone)
         return Planet_Composition;

      function Random_Planet_Density
        (Composition : Planet_Composition;
         Mass        : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Planet_Rotation
        (Mass  : Non_Negative_Real;
         Orbit : Non_Negative_Real;
         Year  : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Planet_Tilt
        return Real;

      function Random_Atmospheric_Class
        (Zone : Orbit_Zone;
         Mass : Non_Negative_Real)
         return Atmosphere_Class;

      function Random_Surface_Pressure
        (Class   : Atmosphere_Class;
         Gravity : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Atmosphere
        (Zone        : Planetary_Zone;
         Composition : Rocky_Planet)
         return Hera.Atmosphere.Configure.Atmosphere_Builder;

   end Planet_Tables;

   -------------------
   -- Planet_Tables --
   -------------------

   package body Planet_Tables is

      type Mass_Parameters is
         record
            Base   : Non_Negative_Real;
            Random : Natural;
         end record;

      function Z return Mass_Parameters is (0.0, 0);
      function K (N : Non_Negative_Real) return Mass_Parameters is (N, 0);
      function R (N : Positive) return Mass_Parameters is (0.0, N);

      type Mass_Table is array (2 .. 12, Planetary_Zone) of Mass_Parameters;
      Planet_Mass : constant Mass_Table :=
                      (2  => (others => Z),
                       3  => (Black => Z, others => K (0.1)),
                       4  => (K (0.1), K (0.2), K (0.2), K (0.1)),
                       5  => (K (0.2), K (0.5), K (0.5), K (0.2)),
                       6  => (K (0.3), K (0.8), K (0.8), K (0.1)),
                       7  => (K (0.5), K (1.0), R (5), K (0.4)),
                       8  => (K (0.8), K (1.2), R (5), K (0.5)),
                       9  => (K (1.0), K (1.5), R (10), R (5)),
                       10 => (K (1.5), K (2.0), R (50), R (10)),
                       11 => (R (1), R (50), R (50), R (50)),
                       12 => (R (50), R (100), R (100), R (100)));

      type Zone_Composition is
        array (Planetary_Zone) of Planet_Composition;

      type Composition_Parameters is
         record
            Mass        : Non_Negative_Real;
            Composition : Zone_Composition;
         end record;

      Composition_Table         : constant array (Positive range <>)
        of Composition_Parameters :=
          ((0.1, (Rock_Iron, Rock, Rock_Ice, Ice)),
           (0.4, (Rock_Iron, Rock_Iron, Rock, Rock_Ice)),
           (0.6, (Rock_Iron, Rock_Iron, Rock_Iron, Rock_Ice)),
           (1.1, (Rock_Iron, Rock_Iron, Rock_Iron, Gaseous)),
           (10.0, (Rock_Iron, Rock_Iron, Gaseous, Gaseous)),
           (50.0, (Gaseous, Gaseous, Gaseous, Gaseous)),
           (100.0, (Gaseous, Gaseous, Hydrogen, Hydrogen)),
           (1000.0, (others => Hydrogen)));

      Density_Table                                : constant array
        (Planet_Composition, 1 .. 3, 1 .. 2) of Real :=
            (Hydrogen  => (others => (0.19, 0.21)),
             Gaseous   => (others => (0.2, 0.3)),
             Ice       => (others => (0.1, 0.2)),
             Rock      => (others => (0.6, 0.7)),
             Rock_Ice  => (others => (0.3, 0.5)),
             Rock_Iron => ((0.5, 0.6), (1.0, 1.6), (1.0, 2.5)));

      -----------------
      -- Composition --
      -----------------

      function Composition
        (Mass : Non_Negative_Real;
         Zone : Orbit_Zone)
         return Planet_Composition
      is
      begin
         for Item of Composition_Table loop
            if Mass < Item.Mass then
               return Item.Composition (Zone);
            end if;
         end loop;
         return Composition_Table (Composition_Table'Last).Composition (Zone);
      end Composition;

      -----------------------
      -- Random_Atmosphere --
      -----------------------

      function Random_Atmosphere
        (Zone        : Planetary_Zone;
         Composition : Rocky_Planet)
         return Hera.Atmosphere.Configure.Atmosphere_Builder
      is
         use Hera.Atmosphere;
         Atm : Hera.Atmosphere.Configure.Atmosphere_Builder;
         type Component_Array is
           array (Positive range <>) of Hera.Atmosphere.Atmospheric_Gas;
         Main : constant Component_Array :=
                  (case Composition is
                      when Rock | Rock_Iron =>
                     (case Zone is
                         when Yellow       => (CO2, N2, SO2),
                         when Green | Blue => (CO2, N2, CH4),
                         when Black        => (1 => H2)),
                      when Ice | Rock_Ice   =>
                     (case Zone is
                         when Yellow | Green   =>
                        (raise Constraint_Error with
                           "ice planet in yellow or green zone"),
                         when Blue             => (CO2, CH4),
                         when Black            => (1 => H2)));

         Trace : constant Component_Array :=
                   (case Composition is
                       when Rock | Rock_Iron =>
                      (case Zone is
                          when Yellow       => (Ar, Cl2, F2),
                          when Green | Blue => (Ar, NH3, SO2, Cl2, F2),
                          when Black        => (1 => He)),
                       when Ice | Rock_Ice   =>
                      (case Zone is
                          when Yellow | Green   =>
                         (raise Constraint_Error with
                            "ice planet in yellow or green zone"),
                          when Blue             => (Ar, N2, NH3),
                          when Black            => (1 => He)));

         Total : Unit_Real := 0.0;

         procedure Add (Components : Component_Array;
                        Count      : Positive;
                        Scale      : Unit_Real);

         ---------
         -- Add --
         ---------

         procedure Add (Components : Component_Array;
                        Count      : Positive;
                        Scale      : Unit_Real)
         is
         begin
            for Gas of Components loop
               declare
                  Partial : constant Unit_Real :=
                              Real'Min
                                (Hera.Random.D (Count) * Scale, 1.0 - Total);
               begin
                  if Partial > 0.0 then
                     Atm.Add_Component (Gas, Partial);
                     Total := Total + Partial;
                  end if;
               end;
            end loop;
         end Add;

      begin
         Add (Main, 1, 0.1);
         Add (Trace, 2, 0.01);

         Atm.Normalize (Hera.Atmosphere.Configure.Change_First_Gas);

--           declare
--              Item : Atmospheric_Component renames
--                       Atm.List (Atm.List.First);
--           begin
--              Item.Partial := Item.Partial + 1.0 - Total;
--           end;
--
--           Atmospheric_Sorting.Sort (Atm.List);

         return Atm;
      end Random_Atmosphere;

      ------------------------------
      -- Random_Atmospheric_Class --
      ------------------------------

      function Random_Atmospheric_Class
        (Zone : Orbit_Zone;
         Mass : Non_Negative_Real)
         return Atmosphere_Class
      is
         Close      : constant Boolean := Zone in Yellow | Green;
         Base_Class : constant Atmosphere_Class :=
                        (if Mass <= 0.3
                         then (if Close then None else Trace)
                         elsif Mass <= 0.5
                         then (if Close then Trace else Thin)
                         elsif Mass <= 0.7 then Thin
                         elsif Mass <= 0.9 then Standard
                         elsif Mass <= 1.3
                         then (if Close then Standard else Dense)
                         else Dense);
         Step_Roll  : constant Unit_Real := Hera.Random.Unit_Random;
         Class      : constant Atmosphere_Class :=
                        (if Step_Roll <= 0.05 and then Base_Class > None
                         then Atmosphere_Class'Pred (Base_Class)
                         elsif Step_Roll >= 0.95 and then Base_Class < Dense
                         then Atmosphere_Class'Succ (Base_Class)
                         else Base_Class);
      begin
         return Class;
      end Random_Atmospheric_Class;

      ---------------------------
      -- Random_Planet_Density --
      ---------------------------

      function Random_Planet_Density
        (Composition : Planet_Composition;
         Mass        : Non_Negative_Real)
         return Non_Negative_Real
      is
         Index : constant Positive :=
                   (if Mass < 1.0 then 1
                    elsif Mass < 2.0 then 2
                    else 3);
         Low   : constant Non_Negative_Real :=
                   Density_Table (Composition, Index, 1);
         High  : constant Non_Negative_Real :=
                   Density_Table (Composition, Index, 2);
      begin
         return Hera.Random.Unit_Random * (High - Low) + Low;
      end Random_Planet_Density;

      ------------------------
      -- Random_Planet_Mass --
      ------------------------

      function Random_Planet_Mass
        (Zone : Planetary_Zone)
         return Non_Negative_Real
      is
         Roll       : constant Positive := Hera.Random.DR;
         Parameters : constant Mass_Parameters :=
                        Planet_Mass (Roll, Zone);
         Mass       : constant Non_Negative_Real :=
                        (Parameters.Base
                         + Real (Parameters.Random) * Hera.Random.D6)
                        * (1.0 - Real (Hera.Random.D (2)) / 100.0);
      begin
         if Mass >= 300.0
           and then Hera.Random.Unit_Random < 0.5
         then
            return Mass * Real (Hera.Random.D (2));
         else
            return Mass;
         end if;
      end Random_Planet_Mass;

      ----------------------------
      -- Random_Planet_Rotation --
      ----------------------------

      function Random_Planet_Rotation
        (Mass  : Non_Negative_Real;
         Orbit : Non_Negative_Real;
         Year  : Non_Negative_Real)
         return Non_Negative_Real
      is
         use Hera.Random;
         N    : constant Positive :=
                  (if Mass <= 0.5 then 6
                   elsif Mass < 5.0 then 5
                   elsif Mass < 50.0 then 4
                   else 3);
         Base : constant Non_Negative_Real :=
                  Real (D (N)) * (0.8 + Hera.Random.Unit_Random * 0.4);
      begin
         if Mass < 10.0 then
            if Orbit < 0.3 then
               return Year * Hera.Solar_System.Earth_Sidereal_Year;
            elsif Orbit < 0.4 then
               return Base * D6 * 10.0;
            elsif Orbit < 0.5 then
               return Base * D6;
            else
               return Base;
            end if;
         else
            return Base;
         end if;
      end Random_Planet_Rotation;

      ------------------------
      -- Random_Planet_Tilt --
      ------------------------

      function Random_Planet_Tilt
        return Real
      is
         use Hera.Random;
      begin
         case D6 is
            when 1 =>
               return D6;
            when 2 | 3 =>
               return 10.0 + D (2);
            when 4 | 5 =>
               return 20.0 + D (2);
            when 6 =>
               declare
                  Tilt : constant Real := D (2) * 10.0;
               begin
                  if Tilt > 90.0 then
                     return 90.0 - Tilt;
                  else
                     return Tilt;
                  end if;
               end;
         end case;
      end Random_Planet_Tilt;

      -----------------------------
      -- Random_Surface_Pressure --
      -----------------------------

      function Random_Surface_Pressure
        (Class   : Atmosphere_Class;
         Gravity : Non_Negative_Real)
         return Non_Negative_Real
      is
         use Hera.Random;
      begin
         return (case Class is
                    when None     => 0.0,
                    when Trace    =>
                      Gravity * D (2) * 0.01,
                    when Thin     =>
                      Gravity * D6 * 0.1,
                    when Standard =>
                      Gravity * D (3) * 0.2,
                    when Dense    =>
                      Gravity * D (2) * 10.0);
      end Random_Surface_Pressure;

   end Planet_Tables;

   procedure Create_Planet
     (Star  : Hera.Stars.Star_Type;
      Index : Positive;
      Zone  : Planetary_Zone;
      Orbit : Non_Negative_Real)
   is
      use Hera.Elementary_Functions;
      use all type Hera.Atmosphere.Atmospheric_Gas;
      subtype Atmosphere is Hera.Atmosphere.Configure.Atmosphere_Builder;

      Earthlike_Fudge : constant Boolean :=
                          (Zone = Green
                           and then Hera.Random.Unit_Random < 0.75);

      Name            : constant String :=
                          Star.Name & " "
                          & WL.Numerics.Roman.Roman_Image (Index);
      Year            : constant Non_Negative_Real :=
                          Sqrt (Orbit ** 3 /
                                (Star.Mass / Hera.Solar_System.Solar_Mass));
      Mass            : constant Non_Negative_Real :=
                          (if Earthlike_Fudge
                           then Unit_Clamp
                             (Hera.Random.Normal_Random (0.1) + 0.5) + 0.5
                           else Planet_Tables.Random_Planet_Mass (Zone));
      Composition     : constant Planet_Composition :=
                          (if Earthlike_Fudge
                           then Rock_Iron
                           else Planet_Tables.Composition (Mass, Zone));
      Gas_Giant       : constant Boolean :=
                          Composition in Gaseous | Hydrogen;
      Density         : constant Non_Negative_Real :=
                          Planet_Tables.Random_Planet_Density
                            (Composition, Mass);
      Radius          : constant Non_Negative_Real :=
                          (Mass / Density) ** (1.0 / 3.0);
      Gravity         : constant Non_Negative_Real := Radius * Density;
      Smoothness      : constant Natural :=
                          Natural (Gravity * 4.0);
      Elevation_Range : constant Natural :=
                          Natural
                            ((Hera.Random.Normal_Random (0.1) + 2.0) * 25.0);
      Day             : constant Non_Negative_Real :=
                          Planet_Tables.Random_Planet_Rotation
                            (Mass  => Mass,
                             Orbit => Orbit,
                             Year  => Year);
      Tilt            : constant Real :=
                          Planet_Tables.Random_Planet_Tilt;

      Atmospheric_Class   : constant Atmosphere_Class :=
                              Planet_Tables.Random_Atmospheric_Class
                                (Zone, Mass);
      Primordial_Pressure : constant Non_Negative_Real :=
                              Planet_Tables.Random_Surface_Pressure
                                (Atmospheric_Class, Gravity);
      Primordial_Atm      : constant Atmosphere :=
          (if Composition in Rocky_Planet
           and then Atmospheric_Class /= None
           then Planet_Tables.Random_Atmosphere
             (Zone, Composition)
           else Hera.Atmosphere.Configure.Vacuum);
      Base_Temperature    : constant Non_Negative_Real :=
                              (Star.Luminosity ** 0.25) * 280.0
                              / Sqrt (Orbit);
      Initial_Temp        : constant Non_Negative_Real :=
                              Real'Max
                                (1.0,
                                 Base_Temperature
                                 - (case Atmospheric_Class is
                                      when None | Trace    => 0.0,
                                      when Thin | Standard => 5.0,
                                      when Dense           => 20.0));

      Primordial_Temp : constant Non_Negative_Real :=
                          Initial_Temp
                            + Primordial_Atm.Partial (CO2) * 100.0;

      Life_Bearing : constant Boolean :=
                       (Primordial_Temp in 253.0 .. 323.0
                        and then Atmospheric_Class >= Thin);

      Life_Complexity : constant Life_Complexity_Type :=
                          (if not Life_Bearing
                           then None
                           elsif Star.Age <= 1.0e9
                           then Prebiotic
                           elsif Star.Age <= 2.0e9
                           then Single_Celled
                           elsif Star.Age <= 3.0e9
                           then Plants
                           else Multicellular);

      Current_Atm         : Atmosphere := Primordial_Atm;
      Final_Atm           : Hera.Atmosphere.Atmosphere_Type;
      Current_Pressure    : Non_Negative_Real := Primordial_Pressure;
      Current_Temperature : Non_Negative_Real := Primordial_Temp;

      Terrain             : array (Planet_Terrain) of Unit_Real :=
                              (others => 0.0);

      Hydrosphere         : Unit_Real renames Terrain (Ocean);
      Climate             : Planet_Climate;
      Habitability        : Unit_Real;
   begin

      if Life_Bearing
        and then Life_Complexity >= Plants
      then
         Current_Atm.Scale_Component (CO2, 0.01);
         Current_Atm.Scale_Component (CH4, 0.0);
         Current_Atm.Add_Component (O2, Hera.Random.D6 * 5.0 / 100.0);
         Current_Atm.Normalize (Hera.Atmosphere.Configure.Change_All_Gases);

--           for Item of Current_Atm.List loop
--              if Item.Gas = CO2 then
--                 Item.Partial := Item.Partial / 100.0;
--              elsif Item.Gas = CH4 then
--                 Item.Partial := 0.0;
--              end if;
--           end loop;
--           Add_Component (Current_Atm, O2, D6 * 5.0 / 100.0);
--           Atmospheric_Sorting.Sort (Current_Atm.List);
         Current_Pressure := Current_Pressure / 2.0;
      end if;

      Final_Atm := Current_Atm.To_Atmosphere (Current_Pressure);

      Current_Temperature := Current_Temperature
        + Current_Atm.Partial (CO2) * 100.0;

--        for Item of Current_Atm.List loop
--           if Item.Gas = CO2 then
--          Current_Temperature := Current_Temperature + Item.Partial * 100.0;
--           end if;
--        end loop;

      declare
         Base_Hydrosphere : constant Real := Hera.Random.D (2)
                              + (if Mass > 1.25 then 1.0 else 0.0)
                              - (if Mass < 0.75 then 1.0 else 0.0)
                              + (if Current_Temperature in 290.0 .. 320.0
                                 then 1.0 else 0.0)
                              - (if Current_Temperature > 250.0
                                 and then Current_Temperature <= 270.0
                                 then 1.0 else 0.0)
                              - (if Current_Temperature in 220.0 .. 250.0
                                 then 2.0 else 0.0);
      begin
         Hydrosphere :=
           (if Current_Temperature < 220.0
            or else Current_Temperature > 370.0
            then 0.0
            else Unit_Clamp (Base_Hydrosphere / 12.0))
             * (if Current_Temperature > 320.0 then 0.5 else 1.0);
      end;

      if not Gas_Giant then
         declare
            Land : constant Unit_Real := 1.0 - Hydrosphere;
         begin
            Terrain (Desert) := Land ** 2;

            if Current_Temperature > 320.0 then
               Terrain (Ice) := 0.0;
               Terrain (Tundra) := 0.0;
            elsif Current_Temperature < 220.0 then
               Terrain (Ice) := Land;
            else
               Terrain (Ice) :=
                 Real'Min (Land,
                           (320.0 - Current_Temperature) / 300.0);
               Terrain (Tundra) :=
                 Real'Min (Land - Terrain (Ice),
                           (320.0 - Current_Temperature) / 500.0);
            end if;

            Terrain (Mountains) :=
              Real'Min (Land, Mass * 0.05);
         end;
      end if;

      Climate :=
        (if Gas_Giant
         then Jovian
         elsif Current_Pressure = 0.0
         then Airless
         elsif Terrain (Ice) > 0.9
         then Iceball
         elsif Hydrosphere > 0.9
         then Water
         elsif Hydrosphere < 0.1
         then Desert
         elsif Atmospheric_Class = Dense
         then Venusian
         elsif Atmospheric_Class = Thin
         then Martian
         elsif Life_Complexity > None
         then Temperate
         elsif Current_Temperature < 270.0
         then Iceball
         else Desert);

      declare
         Ideal_Tmp  : constant := 285.0;
         Std_Dev_Tmp : constant := 25.0;
         Tmp_Factor : constant Unit_Real :=
                        Unit_Clamp
                          (Exp
                             (-(Current_Temperature - Ideal_Tmp) ** 2
                              / (2.0 * Std_Dev_Tmp ** 2)));
         Std_Dev_Oxygen : constant := 0.2;
         function Oxygen_Factor (Partial : Non_Negative_Real) return Unit_Real
         is (Unit_Clamp
             (Exp
              (-(Partial - 0.2) ** 2
               / (2.0 * Std_Dev_Oxygen ** 2))));

         Atm_Factor : Unit_Real := 1.0;

         function Check_Component
           (Gas              : Hera.Atmosphere.Atmospheric_Gas;
            Partial_Pressure : Non_Negative_Real)
            return Unit_Real;

         ---------------------
         -- Check_Component --
         ---------------------

         function Check_Component
           (Gas              : Hera.Atmosphere.Atmospheric_Gas;
            Partial_Pressure : Non_Negative_Real)
           return Unit_Real
         is
         begin
            return (case Gas is
                       when O2                   =>
                         Oxygen_Factor (Partial_Pressure),
                       when Cl2 | F2 | NH3 | SO2 =>
                      (if Partial_Pressure > 0.001
                       then 0.0 else 1.0),
                       when CH4                  =>
                         Unit_Clamp (1.0 - Partial_Pressure * 10.0),
                       when CO2                  =>
                         Unit_Clamp (1.0 - Partial_Pressure * 10.0),
                       when H2 | He | N2 | Ar    =>
                         1.0);
         end Check_Component;

      begin

         for Gas in Hera.Atmosphere.Atmospheric_Gas loop
            declare
               This_Factor : constant Unit_Real :=
                               Check_Component
                                 (Gas, Final_Atm.Partial_Pressure (Gas));
            begin
               Atm_Factor := Real'Min (Atm_Factor, This_Factor);
            end;
         end loop;

         Habitability :=
           Atm_Factor *
             (case Climate is
                 when Airless      => 0.0,
                 when Desert       =>
                   Tmp_Factor * (Hydrosphere + 0.5),
                 when Iceball      => 0.0,
                 when Martian      => 0.0,
                 when Temperate    => Tmp_Factor,
                 when Venusian     => 0.0,
                 when Water        => Tmp_Factor,
                 when Jovian       => 0.0);
      end;

--        if Log_Generation then
--           Ada.Text_IO.Put ("  " & Name);
--           Ada.Text_IO.Set_Col (16);
--           Ada.Text_IO.Put
--             (case Zone is
--                 when Yellow => "Yellow",
--                 when Green  => "Green",
--                 when Blue   => "Blue",
--                 when Black  => "Black");
--           Ada.Text_IO.Set_Col (24);
--           Ada.Text_IO.Put
--             (case Composition is
--                 when Hydrogen  => "Hydrogen",
--                 when Gaseous   => "Gas",
--                 when Ice       => "Ice",
--                 when Rock      => "Rock",
--                 when Rock_Ice  => "Rock-Ice",
--                 when Rock_Iron => "Rock-Iron");
--
--           Ada.Text_IO.Set_Col (36);
--           Put (8, Orbit);
--           Put (8, Mass);
--           Put (8, Density);
--           Put (8, Radius);
--           Put (8, Gravity);
--           Put (8, Year);
--           Put (8, Day);
--
--           if Composition not in Hydrogen | Gaseous then
--              if Hydrosphere = 0.0 then
--                 Put (8, " -");
--              else
--                 Put (8, Hydrosphere * 100.0);
--              end if;
--           else
--              Put (8, " -");
--           end if;
--
--           Put (8, Current_Temperature - 273.0);
--
--           if Composition not in Hydrogen | Gaseous
--             and then Life_Bearing
--           then
--              Put (16,
--                   (case Life_Complexity is
--                       when None          => "none",
--                       when Prebiotic     => "prebiotic",
--                       when Single_Celled => "single-celled",
--                       when Plants        => "plants",
--                       when Multicellular => "multi-cellular"));
--           else
--              Put (16, "none");
--           end if;
--
--           if Composition not in Hydrogen | Gaseous then
--              Put (8,
--                   (case Atmospheric_Class is
--                       when None     => "None",
--                       when Trace    => "Trace",
--                       when Thin     => "Thin",
--                       when Standard => "Std",
--                       when Dense    => "Dense"));
--           else
--              Put (8, " - ");
--           end if;
--
--           Put (8, Habitability);
--
--           Put (8, Current_Pressure);
--
--           if not Gas_Giant then
--              for Item of Current_Atm.List loop
--                 Ada.Text_IO.Put (" " & Item.Gas'Image);
--                 Ada.Text_IO.Put
--                   (Natural'Image (Natural (Item.Partial * 100.0)));
--              end loop;
--
--              for T in Terrain'Range loop
--                 if Terrain (T) > 0.0 then
--                    Ada.Text_IO.Put (" " & T'Image);
--                    Ada.Text_IO.Put
--                      (Natural'Image (Natural (Terrain (T) * 100.0)));
--                 end if;
--              end loop;
--           end if;
--           Ada.Text_IO.New_Line;
--        end if;

      declare
         Planet : Root_Planet_Type := Root_Planet_Type'
           (Hera.Star_Systems.Root_Star_System_Entity with
            Composition         => Composition,
            Atmosphere          => Final_Atm,
            Climate             => To_Climate_Type (Climate),
            Gas_Giant           => Gas_Giant,
            Habitability        => Habitability,
            Colonized           => False,
            Average_Temperature => Current_Temperature,
            Hydrosphere         => Hydrosphere,
            Life_Complexity     => Life_Complexity,
            Smoothness          => Smoothness,
            Elevation_Range     => Elevation_Range,
            Sea_Level           =>
              Natural (Real (Elevation_Range) * Hydrosphere),
            Production          => <>,
            Consumption         => <>);

      begin
         Planet.Initialize_Star_System_Entity
           (Name            => Name,
            System          => Star.System,
            Version         => Planet_Version,
            Mass            => Mass * Hera.Solar_System.Earth_Mass,
            Primary         => Star,
            Orbit           => Orbit * Hera.Solar_System.Earth_Orbit,
            Epoch           => Hera.Calendar.Start,
            Radius          => Radius * Hera.Solar_System.Earth_Radius,
            Density         => Density * Hera.Solar_System.Earth_Density,
            Rotation_Period => Day * 3600.0,
            Tilt            => Tilt,
            Surface_Gravity => Gravity * Hera.Solar_System.Earth_Gravity);
         Planet.New_Planet;
      end;
   end Create_Planet;

end Hera.Planets.Configure;
