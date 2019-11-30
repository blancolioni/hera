with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

with WL.String_Maps;

package body Hera.Profiling is

   type Profile_Entry is
      record
         Name   : Ada.Strings.Unbounded.Unbounded_String;
         Start  : Ada.Calendar.Time;
         Timing : Duration;
         Count  : Natural;
      end record;

   package Profile_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Profile_Entry);

   function Higher_Cost (Left, Right : Profile_Entry) return Boolean
   is (Left.Timing > Right.Timing);

   package Profile_Sorting is
     new Profile_Lists.Generic_Sorting (Higher_Cost);

   package Profile_Maps is
     new WL.String_Maps (Profile_Entry);

   Profile_Map : Profile_Maps.Map;

   ----------------------
   -- Iterate_Profiles --
   ----------------------

   procedure Iterate_Profiles
     (Process : not null access
        procedure (Name : String;
                   Timing : Duration;
                   Count : Positive))
   is
      List : Profile_Lists.List;
   begin
      for Item of Profile_Map loop
         List.Append (Item);
      end loop;
      Profile_Sorting.Sort (List);
      for Item of reverse List loop
         Process (Ada.Strings.Unbounded.To_String (Item.Name),
                  Item.Timing, Item.Count);
      end loop;
   end Iterate_Profiles;

   ---------------------
   -- Report_Profiles --
   ---------------------

   procedure Report_Profiles is

      Longest : Natural := 0;

      procedure Report_Timing
        (Name   : String;
         Timing : Duration;
         Count  : Positive);

      -------------------
      -- Report_Timing --
      -------------------

      procedure Report_Timing
        (Name   : String;
         Timing : Duration;
         Count  : Positive)
      is
      begin
         Ada.Text_IO.Put (Name);
         Ada.Text_IO.Set_Col (Ada.Text_IO.Count (Longest + 2));
         Ada.Float_Text_IO.Put (Float (Timing), 8, 3, 0);
         Ada.Integer_Text_IO.Put (Count, 8);
         Ada.Text_IO.New_Line;
      end Report_Timing;

   begin
      for Item of Profile_Map loop
         Longest := Natural'Max
           (Ada.Strings.Unbounded.Length (Item.Name), Longest);
      end loop;

      Iterate_Profiles (Report_Timing'Access);
   end Report_Profiles;

   -----------
   -- Start --
   -----------

   procedure Start (Name : String) is
   begin
      if Profile_Map.Contains (Name) then
         Profile_Map (Name).Start := Ada.Calendar.Clock;
      else
         Profile_Map.Insert
           (Name,
            Profile_Entry'
              (Name   => Ada.Strings.Unbounded.To_Unbounded_String (Name),
               Start  => Ada.Calendar.Clock,
               Timing => 0.0,
               Count  => 0));
      end if;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Name : String) is
      use type Ada.Calendar.Time;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      P   : Profile_Entry renames Profile_Map (Name);
   begin
      P.Timing := P.Timing + (Now - P.Start);
      P.Count := P.Count + 1;
   end Stop;

end Hera.Profiling;
