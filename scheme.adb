with Ada.Integer_Text_Io;
with Ada.Text_Io;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;

use Ada.Integer_Text_Io;
use Ada.Text_Io;
use Ada.Characters.Handling;
use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;

procedure Scheme is

   procedure Stderr (Str: in String) is
   begin
      Set_Output(Standard_Error);
      Put_Line(Str);
   end;


   package U_Str renames Ada.Strings.Unbounded;

   -- MODEL ---------------------------------------------------------------

   type Object_Type is (Int);

   type Object_Data is record
      Int : Integer;
   end record;

   type Object is record
      O_Type : Object_Type;
      Data : Object_Data;
   end record;

   type Access_Object is access Object;

   function Allowc_Object return Access_Object is
      Obj : Access_Object;
   begin
      Obj := new Object;
      return Obj;
   end;

   function Make_Integer (Value : Integer) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Allowc_Object;
      Obj.all.O_Type := Int;
      Obj.all.Data.Int := Value;
      return Obj;
   end;

   -- READ ----------------------------------------------------------------

   function Read return Access_Object is

      Str : U_Str.Unbounded_String;
      I, Sign : Integer := 1;
      Num : Integer := 0;

      function Is_Delimiter (C : Character) return Boolean is
      begin
         return C = ' ' or else C = '('
           or else C = ')' or else C = '"';
      end;

      function Is_Space (C : Character) return Boolean is
      begin
         return C = ' ';
      end;

   begin
      Str := Get_Line;

      while I <= Length(Str) loop
         if Is_Space(Element(Str, I)) then
            Stderr("Inside is_space branch of while loops logic.");
            I := I + 1;
         elsif Is_Digit(Element(Str, I)) or else Element(Str, I) = '-' then
            if Element(Str, I) = '-' then
               Sign := -1;
            elsif I /= 1 then
               I := I - 1;
            end if;

            while Length(Str) >= I and then Is_Digit(Element(Str, I)) loop
               Num := (Num * 10);
               Num := Num + (Character'Pos(Element(Str, I)) - Character'Pos('0'));
               I := I + 1;
            end loop;
            Num := Num * Sign;

            if I = Length(Str) + 1 or else Is_Delimiter(Element(Str, I)) then
               return Make_Integer(Num);
            else
               Stderr("Number not followed by a delimiter.");
               raise Constraint_Error;
            end if;
         else
            Stderr("Read illegal state.");
            raise Constraint_Error;
         end if;
      end loop;

      Stderr("Uh oh read is returning null");
      return null;
   end;

   -- EVAL ----------------------------------------------------------------

   -- Until we have lists and symbols, just echo
   function Eval (Exp : Access_Object) return Access_Object is
   begin
      return Exp;
   end;

   -- PRINT ---------------------------------------------------------------

   procedure Print (Obj : in Access_Object) is
   begin
      if Obj = null then
         Stderr("Null object type.");
         raise Constraint_Error;
      end if;

      case Obj.all.O_Type is
         when Int =>
            Put(Obj.all.Data.Int, Width => 0);
         when others =>
            Stderr("Cannot write unknown data type.");
            raise Constraint_Error;
      end case;
   end;

begin

   -- REPL ----------------------------------------------------------------

   Ada.Text_Io.Put_Line("Welcome to Bootstrap Scheme -- Ada version.");

   loop
      Put("> ");
      Print(Eval(Read));
      New_Line;
   end loop;

end;


-- MUSIC ------------------------------------------------------------------

-- Lifer's Group, Grand Puba
