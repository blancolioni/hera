with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Text_IO;

with WL.Heaps;
with WL.Processes;

with Hera.Elementary_Functions;
with Hera.Random;
with Hera.Real_Images;

with Hera.Objects;
with Hera.Planets;

--  with Hera.Solar_System;
--  with Hera.Stars.Tables;
--
--  with Hera.Calendar;

--  with Hera.Planets.Surfaces;
with Hera.Star_Systems.Configure;

package body Hera.Galaxy.Configure is

--     Star_System_Version : constant := 1;

   function Random_Star_Mass return Non_Negative_Real
     with Unreferenced;

   function Right_Ascension_To_Radians
     (Hours, Minutes, Seconds : Real)
      return Real
     with Unreferenced;

   function Declination_To_Radians
     (Degrees, Minutes, Seconds : Real)
      return Real
     with Unreferenced;

   procedure String_To_Real_3
     (Text    : String;
      X, Y, Z : out Real)
     with Unreferenced;

   function Random_Coordinate
     return Real
   is (Hera.Random.Normal_Random (0.5));

   ----------------------------
   -- Declination_To_Radians --
   ----------------------------

   function Declination_To_Radians
     (Degrees, Minutes, Seconds : Real)
      return Real
   is
      Result : constant Real :=
                 (abs Degrees + Minutes / 60.0 + Seconds / 3600.0)
                 * Ada.Numerics.Pi / 180.0;
   begin
      if Degrees < 0.0 then
         return -Result;
      else
         return Result;
      end if;
   end Declination_To_Radians;

   ---------------------
   -- Generate_Galaxy --
   ---------------------

   procedure Generate_Galaxy
     (Number_Of_Systems  : Positive;
      Radius_X           : Non_Negative_Real;
      Radius_Y           : Non_Negative_Real;
      Radius_Z           : Non_Negative_Real;
      Names              : WL.Random.Names.Name_Generator)
   is
      use Hera.Elementary_Functions;
      use Hera.Real_Images;

      Volume     : constant Non_Negative_Real :=
                     4.0 * Ada.Numerics.Pi / 3.0
                       * Radius_X * Radius_Y * Radius_Z;
      Volume_Per_Star : constant Non_Negative_Real :=
                          Volume
                            / Non_Negative_Real (Number_Of_Systems);
      Radius_Per_Star : constant Non_Negative_Real :=
                          (3.0 * Volume_Per_Star
                           / (4.0 * Ada.Numerics.Pi)) ** (1.0 / 3.0);
      Minimum_Distance : constant Non_Negative_Real :=
                           Radius_Per_Star / 4.0;

      package Star_System_Heaps is
        new WL.Heaps (Non_Negative_Real, Positive, ">");

      type Generated_Star_Record is
         record
            X, Y, Z : Real;
            System  : Hera.Star_Systems.Star_System_Type;
            Nearby  : Star_System_Heaps.Heap;
         end record;

      package Generated_Star_Vectors is
        new Ada.Containers.Vectors (Positive, Generated_Star_Record);

      Vector : Generated_Star_Vectors.Vector;

      function Distance
        (From, To : Generated_Star_Record)
         return Non_Negative_Real
      is ((From.X - To.X) ** 2
          + (From.Y - To.Y) ** 2
          + (From.Z - To.Z) ** 2);

      Process : WL.Processes.Process_Type;

   begin

      if Volume = 0.0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "At least one of galaxy-radius, galaxy-radius-[xyz]"
            & " must have a value");
         raise Program_Error;
      end if;

      Ada.Text_IO.Put_Line
        ("Galaxy size: ("
         & Approximate_Image (Radius_X)
         & ","
         & Approximate_Image (Radius_Y)
         & ","
         & Approximate_Image (Radius_Z)
         & ")");
      Ada.Text_IO.Put_Line
        ("Volume (ly**3): "
         & Approximate_Image (Volume)
         & " per star: "
         & Approximate_Image (Volume_Per_Star));
      Ada.Text_IO.Put_Line
        ("Average star distance: "
         & Approximate_Image (Radius_Per_Star));
      Ada.Text_IO.Put_Line
        ("Minimum star distance: "
         & Approximate_Image (Minimum_Distance));

      Process.Start_Bar
        (Name            => "Gen coordinates     ",
         Finish          => Number_Of_Systems,
         With_Percentage => True,
         Bar_Length      => 40);

      for I in 1 .. Number_Of_Systems loop

         declare
            Rec : Generated_Star_Record;
            OK  : Boolean := False;
            Min : constant Non_Negative_Real := Minimum_Distance ** 2;
         begin
            while not OK loop
               Rec.X := Random_Coordinate * Radius_X;
               Rec.Y := Random_Coordinate * Radius_Y;
               Rec.Z := Random_Coordinate * Radius_Z;

               OK := True;
               for J in 1 .. I - 1 loop
                  if Distance (Rec, Vector.Element (J)) < Min then
                     OK := False;
                     exit;
                  end if;
               end loop;
            end loop;

            Vector.Append (Rec);

            for J in 1 .. I - 1 loop
               declare
                  D : constant Non_Negative_Real :=
                    Distance (Vector.Element (I), Vector.Element (J));
               begin
                  Vector (I).Nearby.Insert (D, J);
                  Vector (J).Nearby.Insert (D, I);
               end;
            end loop;

            Process.Tick;
         end;
      end loop;

      Process.Finish;

      Process.Start_Bar
        (Name            => "Updating star table ",
         Finish          => Number_Of_Systems,
         With_Percentage => True,
         Bar_Length      => 40);

      for I in 1 .. Number_Of_Systems loop

         declare
            System_Name : constant String :=
              WL.Random.Names.Random_Name (Names);
            Gen         : Generated_Star_Record renames Vector (I);
         begin
            Hera.Star_Systems.Configure.Create_Star_System
              (Name => System_Name,
               X    => Gen.X,
               Y    => Gen.Y,
               Z    => Gen.Z);
            Process.Tick;
         end;
--
--              Solar_Masses : constant Non_Negative_Real :=
--                               Random_Star_Mass;
--              Mass         : constant Non_Negative_Real :=
--                               Solar_Masses * Hera.Solar_System.Solar_Mass;
--              Star_System  : constant Hera.Db.Star_System_Reference :=
--                Hera.Db.Star_System.Create
--                  (Version    => Star_System_Version,
--                   Name       => System_Name,
--                   Identifier => Hera.Identifiers.Next_Identifier,
--                   X          => Gen.X,
--                   Y          => Gen.Y,
--                   Z          => Gen.Z);
--              Radius       : Non_Negative_Real;
--              Luminosity   : Non_Negative_Real;
--              R, G, B      : Unit_Real;
--           begin
--
--              Gen.System :=
--                Hera.Star_Systems.Configure.Create_Star_System
--                  (Name => System_Name,
--                   X    => Gen.X,
--                   Y    => Gen.Y,
--                   Z    => Gen.Z);
--
--              Vector (I).Reference := Star_System;
--
--              Hera.Stars.Tables.Get_Main_Sequence_Info
--                (Solar_Masses => Solar_Masses,
--                 Radius       => Radius,
--                 Luminosity   => Luminosity,
--                 R            => R,
--                 G            => G,
--                 B            => B);
--
--              declare
--                 Volume : constant Non_Negative_Real :=
--                            4.0 * Ada.Numerics.Pi * (Radius ** 3) / 3.0;
--                 Age    : constant Non_Negative_Real :=
--                   4.0e9
--                     * Solar_Masses
--                   / Luminosity
--                   * (Hera.Random.Unit_Random + 0.5);
--                 Temperature : constant Non_Negative_Real :=
--                   (Luminosity ** 0.25)
--                   * Hera.Solar_System.Solar_Surface_Temperature;
--                 Ecosphere   : constant Non_Negative_Real :=
--                   Hera.Elementary_Functions.Sqrt (Luminosity);
--              begin
--
--                 Hera.Db.Star.Create
--                   (Star_System     => Star_System,
--                    Orbiting    => Hera.Db.Null_Star_System_Entity_Reference,
--                    Orbit           => 0.0,
--                    Period          => 0.0,
--                    Epoch           => Hera.Calendar.Clock,
--                    Radius          => Radius,
--                    Density         => Mass / Volume,
--                    Rotation_Period => 3600.0 * 11.0,
--                    Tilt            => 0.0,
--                    Surface_Gravity => 99.9,
--                    Red             => R,
--                    Green           => G,
--                    Blue            => B,
--                    Version         => Star_System_Version,
--                    Name            => System_Name,
--                    Mass            => Mass,
--                    Identifier      => Hera.Identifiers.Next_Identifier,
--                    Luminosity      => Luminosity,
--                    Temperature     => Temperature,
--                    Age             => Age,
--                    Ecosphere       => Ecosphere);
--                 Process.Tick;
--              end;
--           end;
      end loop;

      Process.Finish;

--        Process.Start_Bar
--          (Name            => "Updating distances  ",
--           Finish          => Number_Of_Systems,
--           With_Percentage => True,
--           Bar_Length      => 40);
--
--        for I in 1 .. Number_Of_Systems loop
--           declare
--              H : Star_System_Heaps.Heap := Vector.Element (I).Nearby;
--              Count : Natural := 0;
--           begin
--              while not H.Is_Empty
--                and then Count < Number_Of_Systems / 10
--              loop
--                 declare
--                    Distance : constant Non_Negative_Real :=
--                      H.First_Key;
--                    To       : constant Positive := H.First_Element;
--                 begin
--                    H.Delete_First;
--                    Hera.Db.Star_System_Distance.Create
--                      (From => Vector.Element (I).Reference,
--                       To   => Vector.Element (To).Reference,
--                     Distance => Hera.Elementary_Functions.Sqrt (Distance));
--                    Count := Count + 1;
--                 end;
--              end loop;
--           end;
--
--           Process.Tick;
--
--        end loop;
--
--        Process.Finish;

--        Process.Start_Bar
--          (Name            => "Generating systems  ",
--           Finish          => Number_Of_Systems,
--           With_Percentage => True,
--           Bar_Length      => 40);
--
--        for I in 1 .. Number_Of_Systems loop
--           declare
--              Star_System : constant Hera.Star_Systems.Star_System_Handle :=
--                Hera.Star_Systems.Star_System
--                  (Vector.Element (I).Reference);
--           begin
--              Hera.Star_Systems.Configure.Generate_Star_System
--                (Star_System);
--           end;
--
--           Process.Tick;
--
--        end loop;
--
--        Process.Finish;

--        if False then
--           Process.Start_Bar
--             (Name            => "Generating surfaces ",
--              Finish          => Number_Of_Systems,
--              With_Percentage => True,
--              Bar_Length      => 40);
--
--           for I in 1 .. Number_Of_Systems loop
--              for Planet of
--                Hera.Db.Planet.Select_By_Star_System
--                  (Vector.Element (I).Reference)
--              loop
--                 if not Planet.Gas_Giant then
--                    Hera.Planets.Surfaces.Create_Surface
--                    (Hera.Handles.Planet.Get (Planet.Get_Planet_Reference));
--                 end if;
--              end loop;
--              Process.Tick;
--
--           end loop;
--
--           Process.Finish;
--        end if;

      Hera.Objects.Apply_Updates;

      declare
         Total, Marginal, Habitable, Ideal : Natural := 0;

         procedure Process (Planet : Hera.Planets.Planet_Type);

         -------------
         -- Process --
         -------------

         procedure Process (Planet : Hera.Planets.Planet_Type) is
         begin
            Total := Total + 1;
            if Planet.Habitability > 0.8 then
               Ideal := Ideal + 1;
            elsif Planet.Habitability > 0.6 then
               Habitable := Habitable + 1;
            elsif Planet.Habitability > 0.2 then
               Marginal := Marginal + 1;
            end if;
         end Process;

      begin
         Hera.Planets.Iterate (Process'Access);
         Ada.Text_IO.Put_Line
           ("planets: total" & Total'Image
            & "; marginal" & Marginal'Image
            & "; habitable" & Habitable'Image
            & "; ideal" & Ideal'Image);
      end;

   end Generate_Galaxy;

   ----------------------
   -- Random_Star_Mass --
   ----------------------

   function Random_Star_Mass return Non_Negative_Real is
      Realistic_Star_Masses : constant Boolean := False;
      Seed : constant Real := Hera.Random.Unit_Random;
      Solar_Mass_Count : Real;
   begin
      if Realistic_Star_Masses then
         if Seed <= 0.99 then
            Solar_Mass_Count :=
              0.1 + 6.0 * Seed - 15.0 * Seed ** 2
                + 11.0 * Seed ** 3;
         else
            declare
               X : constant Real := (Seed - 0.99) * 1.0E4;
               A : constant Real := 0.110833;
               B : constant Real := -14.0358;
               C : constant Real := 445.25;
            begin
               Solar_Mass_Count := A * X ** 2 + B * X + C;
            end;
         end if;
      else
         Solar_Mass_Count :=
           Signed_Unit_Clamp (Hera.Random.Normal_Random (0.2))
             + 1.0;
      end if;
      return Solar_Mass_Count;
   end Random_Star_Mass;

   --------------------------------
   -- Right_Ascension_To_Radians --
   --------------------------------

   function Right_Ascension_To_Radians
     (Hours, Minutes, Seconds : Real)
      return Real
   is
   begin
      return (Hours * 15.0 + Minutes / 4.0 + Seconds / 4.0 / 60.0)
        * Ada.Numerics.Pi / 180.0;
   end Right_Ascension_To_Radians;

   ----------------------
   -- String_To_Real_3 --
   ----------------------

   procedure String_To_Real_3
     (Text    : String;
      X, Y, Z : out Real)
   is
      Start : Positive := Text'First;
      Index : Positive := Text'First;
      Result : array (1 .. 3) of Real := (others => 0.0);
      R_Idx  : Positive := 1;
   begin

      while Index <= Text'Last loop
         while Index <= Text'Last
           and then Text (Index) = ' '
         loop
            Index := Index + 1;
         end loop;

         Start := Index;

         while Index <= Text'Last
           and then Text (Index) /= ' '
         loop
            Index := Index + 1;
         end loop;

         if Index > Start then
            Result (R_Idx) := Real'Value (Text (Start .. Index - 1));
            R_Idx := R_Idx + 1;
         end if;
      end loop;

      X := Result (1);
      Y := Result (2);
      Z := Result (3);

   end String_To_Real_3;

end Hera.Galaxy.Configure;
