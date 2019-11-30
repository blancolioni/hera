with Ada.Containers.Indefinite_Holders;

with Hera.UI.Sessions;

package body Hera.UI is

   package UI_Holders is
     new Ada.Containers.Indefinite_Holders (UI_Interface'Class);

   Holder : UI_Holders.Holder;

   procedure After_Update is

      procedure Send_After_Update
        (State : State_Interface'Class);

      -----------------------
      -- Send_After_Update --
      -----------------------

      procedure Send_After_Update
        (State : State_Interface'Class)
      is
      begin
         State.After_Update;
      end Send_After_Update;

   begin
      Hera.UI.Sessions.Scan_Active_Sessions
        (Send_After_Update'Access);
   end After_Update;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (Signal : Hera.Signals.Signal_Type) is
   begin
      Current_UI.Broadcast (Signal);
   end Broadcast;

   ---------------
   -- Close_All --
   ---------------

   procedure Close_All is
   begin
      Sessions.Close_All_Sessions;
   end Close_All;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return UI_Interface'Class is
   begin
      return Holder.Element;
   end Current_UI;

   -------------------
   -- On_UI_Started --
   -------------------

   procedure On_UI_Started (UI : UI_Interface'Class) is
   begin
      Holder := UI_Holders.To_Holder (UI);
   end On_UI_Started;

   ------------------
   -- Send_Message --
   ------------------

--     procedure Send_Message
--       (Message : Corporation_Message)
--     is
--        M : Json.Json_Object;
--     begin
--        if Hera.UI.Sessions.Is_Active (Message.Corporation) then
--           if (for some Flag of Message.Flags => Flag) then
--              M.Set_Property ("type", "update-faction");
--              if Message.Flags (Cash_Changed) then
--                 M.Set_Property
--                   ("cash",
--                    Float (Hera.Money.To_Real (Message.Corporation.Cash)));
--              end if;
--              Hera.UI.Sessions.Element (Message.Corporation)
--                .Send_Message (M);
--           end if;
--        end if;
--     end Send_Message;

   ----------------
   -- Send_State --
   ----------------

   procedure Send_State is

      procedure Send_Session_State
        (State : State_Interface'Class);

      ------------------------
      -- Send_Session_State --
      ------------------------

      procedure Send_Session_State
        (State : State_Interface'Class)
      is null;
--           Message : constant Corporation_Message :=
--             New_Message (State.Corporation).Cash_Changed;
--        begin
--           Send_Message (Message);
--        end Send_Session_State;

   begin
      Hera.UI.Sessions.Scan_Active_Sessions
        (Send_Session_State'Access);
   end Send_State;

end Hera.UI;
