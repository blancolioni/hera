with WL.Heaps;
with WL.String_Maps;

with Hera.Money;

with Hera.Updates.Events;

with Hera.Colonies;
with Hera.Managers.Colonies;

with Hera.Installations;
with Hera.Managers.Installations;

package body Hera.Managers is

   package Manager_Queues is
     new WL.Heaps
       (Key_Type     => Hera.Calendar.Time,
        Element_Type => Manager_Type,
        "<"          => Hera.Calendar.">");

   package Manager_Maps is
     new WL.String_Maps (Manager_Type);

   type Autoplayer_Record is
      record
         Corporation : Hera.Corporations.Corporation_Type;
         Managers    : Manager_Maps.Map;
         Active      : Manager_Queues.Heap;
         Started     : Boolean;
      end record;

   type Autoplayer_Access is access Autoplayer_Record;

   type Autoplayer_Update is
     new Hera.Updates.Update_Interface with
      record
         Autoplayer : Autoplayer_Access;
      end record;

   overriding procedure Activate (Update : Autoplayer_Update);

   procedure Load_Objects (Manager : in out Autoplayer_Record);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Autoplayer_Update) is
      use type Hera.Calendar.Time;
      Current_Clock : constant Hera.Calendar.Time := Hera.Calendar.Clock;
      Active_Queue  : Manager_Queues.Heap renames
        Update.Autoplayer.Active;
   begin
      Update.Autoplayer.Corporation.Log
        ("manager activated; cash = "
         & Hera.Money.Show (Update.Autoplayer.Corporation.Cash));

      if not Update.Autoplayer.Started then
         Load_Objects (Update.Autoplayer.all);
         Update.Autoplayer.Started := True;
      end if;

      while not Active_Queue.Is_Empty
        and then Active_Queue.First_Key <= Current_Clock
      loop
         declare
            M : constant Manager_Type := Active_Queue.First_Element;
         begin
            Active_Queue.Delete_First;
            M.Active := False;
            M.Execute;
            if M.Active then
               pragma Assert (M.Next_Update > Current_Clock);
               Active_Queue.Insert (M.Next_Update, M);
            end if;
         end;
      end loop;

      if not Update.Autoplayer.Active.Is_Empty then
         Hera.Updates.Events.Update_At
           (Update.Autoplayer.Active.First_Key, Update);
      end if;

   end Activate;

   ------------------
   -- Load_Objects --
   ------------------

   procedure Load_Objects (Manager : in out Autoplayer_Record) is

      procedure Add_Colony
        (Colony : Hera.Colonies.Colony_Type);

      procedure Add_Installation
        (Installation : Hera.Installations.Installation_Type);

      ----------------
      -- Add_Colony --
      ----------------

      procedure Add_Colony
        (Colony : Hera.Colonies.Colony_Type)
      is
      begin
         if Colony.Has_Warehouse (Manager.Corporation) then
            declare
               M : constant Manager_Type :=
                 Hera.Managers.Colonies.Colony_Manager
                   (Manager.Corporation, Colony);
            begin
               Manager.Managers.Insert (String (Colony.Identifier), M);
               Manager.Active.Insert (Hera.Calendar.Clock, M);
            end;
         end if;
      end Add_Colony;

      ----------------------
      -- Add_Installation --
      ----------------------

      procedure Add_Installation
        (Installation : Hera.Installations.Installation_Type)
      is
         M : constant Manager_Type :=
           Hera.Managers.Installations.Installation_Manager
             (Installation);
      begin
         Manager.Managers.Insert (String (Installation.Identifier), M);
         Manager.Active.Insert (Hera.Calendar.Clock, M);
      end Add_Installation;

   begin
      Hera.Colonies.Iterate
        (Add_Colony'Access);
      Hera.Installations.Scan_By_Owner
        (Manager.Corporation, Add_Installation'Access);
   end Load_Objects;

   ---------------------------
   -- Set_Next_Update_Delay --
   ---------------------------

   procedure Set_Next_Update_Delay
     (Manager      : in out Root_Manager_Type'Class;
      Update_Delay : Duration)
   is
      use type Hera.Calendar.Time;
   begin
      Manager.Set_Next_Update_Time (Hera.Calendar.Clock + Update_Delay);
   end Set_Next_Update_Delay;

   --------------------------
   -- Set_Next_Update_Time --
   --------------------------

   procedure Set_Next_Update_Time
     (Manager     : in out Root_Manager_Type'Class;
      Update_Time : Hera.Calendar.Time)
   is
   begin
      Manager.Active := True;
      Manager.Next_Update := Update_Time;
   end Set_Next_Update_Time;

   -------------------
   -- Start_Manager --
   -------------------

   procedure Start_Manager (Corporation : Hera.Corporations.Corporation_Type)
   is
      Update : constant Autoplayer_Update := Autoplayer_Update'
        (Autoplayer  => new Autoplayer_Record'
           (Corporation => Corporation,
            Managers    => <>,
            Active      => <>,
            Started     => False));
   begin
      Hera.Updates.Events.Update_At
        (Clock  => Hera.Calendar.Clock,
         Update => Update);
   end Start_Manager;

end Hera.Managers;
