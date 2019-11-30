with Hera.Colors;
with Hera.Factions;
with Hera.Phases.Sequence;

package body Hera.UI.Web_UI.Requests is

   function Faction_State
     (Game    : Hera.Game.Game_Type;
      Faction : Faction_Id)
      return Hera.Json.Json_Value'Class;

   function Faction_Senators
     (Game    : Hera.Game.Game_Type;
      Faction : Faction_Id)
      return Hera.Json.Json_Value'Class;

   function Senator_State
     (Game    : Hera.Game.Game_Type;
      Senator : Senator_Id)
      return Hera.Json.Json_Value'Class;

   --------------
   -- Continue --
   --------------

   function Continue
     (Game : in out Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      Game.Next_Step;
      return Game_State (Game);
   end Continue;

   -------------------
   -- Current_Phase --
   -------------------

   function Current_Phase
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property
           ("tag",
              Hera.Phases.Sequence.Phase (Game.Current_Phase).Name);
         Result.Set_Property
           ("localName",
            Game.Local_Text
              (Hera.Phases.Sequence.Phase (Game.Current_Phase).Name));
      end return;
   end Current_Phase;

   -------------------
   -- Faction_Names --
   -------------------

   function Faction_Names
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Array do
         for I in Faction_Id loop
            Result.Append
              (Hera.Json.String_Value
                 (Game.Faction_Name (I)));
         end loop;
      end return;
   end Faction_Names;

   ----------------------
   -- Faction_Senators --
   ----------------------

   function Faction_Senators
     (Game    : Hera.Game.Game_Type;
      Faction : Faction_Id)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Array do
         declare
            Ss : constant Senator_Id_Array :=
              Game.Faction_Senators (Faction);
         begin
            for S of Ss loop
               Result.Append (Senator_State (Game, S));
            end loop;
         end;
      end return;
   end Faction_Senators;

   -------------------
   -- Faction_State --
   -------------------

   function Faction_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Array do
         for I in Faction_Id loop
            Result.Append (Faction_State (Game, I));
         end loop;
      end return;
   end Faction_State;

   -------------------
   -- Faction_State --
   -------------------

   function Faction_State
     (Game    : Hera.Game.Game_Type;
      Faction : Faction_Id)
      return Hera.Json.Json_Value'Class
   is
      F : constant Hera.Factions.Faction_Type'Class :=
        Game.Faction (Faction);
   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property ("name", Game.Faction_Name (Faction));
         Result.Set_Property
           ("votes", Natural (Game.Faction_Votes (Faction)));
         Result.Set_Property
           ("influence", Natural (Game.Faction_Influence (Faction)));
         Result.Set_Property
           ("treasury", Natural (Game.Faction_Treasury (Faction)));
         Result.Set_Property
           ("leader", (if F.Has_Leader then Natural (F.Leader) else 0));
         Result.Set_Property
           ("senators", Faction_Senators (Game, Faction));
         Result.Set_Property
           ("color",
            Hera.Colors.To_Html_Color_String
              (Game.Get_Faction_State (Faction).Color));
      end return;
   end Faction_State;

   -----------------
   -- Fleet_State --
   -----------------

   function Fleet_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property ("total", Natural (Game.Total_Fleet_Count));
      end return;
   end Fleet_State;

   ----------------
   -- Game_State --
   ----------------

   function Game_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is

   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property ("factions", Faction_State (Game));
         Result.Set_Property ("republic", Republic_State (Game));
      end return;
   end Game_State;

   ------------------
   -- Legion_State --
   ------------------

   function Legion_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property ("total", Natural (Game.Total_Legion_Count));
      end return;
   end Legion_State;

   --------------------
   -- Republic_State --
   --------------------

   function Republic_State
     (Game : Hera.Game.Game_Type)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property ("currentPhase", Current_Phase (Game));
         Result.Set_Property
           ("treasury", Natural (Game.Current_Treasury));
         Result.Set_Property
           ("unrest", Natural (Game.Current_Unrest));
         Result.Set_Property
           ("legions", Legion_State (Game));
         Result.Set_Property
           ("fleets", Fleet_State (Game));
         Result.Set_Property
           ("hrao",
            Senator_State (Game, Game.Highest_Ranking_Available_Officer));
      end return;
   end Republic_State;

   -------------------
   -- Senator_State --
   -------------------

   function Senator_State
     (Game    : Hera.Game.Game_Type;
      Senator : Senator_Id)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Result : Hera.Json.Json_Object do
         Result.Set_Property ("id", Natural (Senator));
         Result.Set_Property ("family", Game.Senator_Name (Senator));
         Result.Set_Property
           ("nameAndFaction",
            Game.Senator_Name_And_Faction (Senator));
         Result.Set_Property
           ("faction",
            (if Game.Has_Faction (Senator)
             then Natural (Game.Senator_Faction (Senator))
             else 0));
         Result.Set_Property
           ("factionLeader",
            Game.Is_Faction_Leader (Senator));
         Result.Set_Property ("military", Natural (Game.Military (Senator)));
         Result.Set_Property ("oratory", Natural (Game.Oratory (Senator)));
         Result.Set_Property ("loyalty", Natural (Game.Loyalty (Senator)));
         Result.Set_Property ("influence", Natural (Game.Influence (Senator)));
         Result.Set_Property
           ("knights",
            Game.Get_Senator_State (Senator).Knights);
         Result.Set_Property
           ("popularity", Integer (Game.Popularity (Senator)));
         Result.Set_Property
           ("treasury", Natural (Game.Senator_Treasury (Senator)));
         Result.Set_Property
           ("priorConsul", Game.Is_Prior_Consul (Senator));
         if Game.Has_Office (Senator) then
            declare
               Tag : constant String :=
                 (case Game.Office (Senator) is
                     when Dictator => "DI",
                     when Rome_Consul => "RC",
                     when Field_Consul => "FC",
                     when Censor       => "CE",
                     when Master_Of_Horse => "MH",
                     when Pontifex_Maximus => "PM");
            begin
               Result.Set_Property
                 ("office", Tag);
            end;
         end if;
      end return;
   end Senator_State;

end Hera.UI.Web_UI.Requests;
