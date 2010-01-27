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

   -- MODEL ---------------------------------------------------------------

   type Object_Type is (Int, Bool, Char, Strng, Empty_List, Pair, Symbol);

   type Object;
   type Access_Object is access Object;

   type Pair_Object is record
      Car : Access_Object;
      Cdr : Access_Object;
   end record;

   type Object_Data is record
      Int : Integer;
      Bool : Boolean;
      Char : Character;
      Strng : Unbounded_String;
      Pair : Pair_Object;
      Symbol : Unbounded_String;
   end record;

   type Object is record
      O_Type : Object_Type;
      Data : Object_Data;
   end record;

   function Allowc_Object return Access_Object is
      Obj : Access_Object;
   begin
      Obj := new Object;
      return Obj;
   end;

   True_Singleton : Access_Object;
   False_Singleton : Access_Object;
   The_Empty_List : Access_Object;
   Symbol_Table : Access_Object;

   function Is_Boolean (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Bool;
   end;

   function Is_False (Obj : Access_Object) return Boolean is
   begin
      return Obj = False_Singleton;
   end;

   function Is_True (Obj : Access_Object) return Boolean is
   begin
      return Obj = True_Singleton;
   end;

   function Is_The_Empty_List (Obj : Access_Object) return Boolean is
   begin
      return Obj = The_Empty_List;
   end;

   function Is_Integer (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Int;
   end;

   function Is_Character (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Char;
   end;

   function Is_String (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Strng;
   end;

   function Is_Pair (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Pair;
   end;

   function Is_Symbol (Obj : Access_Object) return Boolean is
   begin
      return Obj.all.O_Type = Symbol;
   end;

   function Cons (Car : Access_Object;
                  Cdr : Access_Object) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Allowc_Object;
      Obj.all.O_Type := Pair;
      Obj.all.Data.Pair.Car := Car;
      Obj.all.Data.Pair.Cdr := Cdr;
      return Obj;
   end;

   function Car (Pair_Obj : Access_Object) return Access_Object is
   begin
      return Pair_Obj.all.Data.Pair.Car;
   end;

   procedure Set_Car (Pair_Obj : in out Access_Object;
                      Val : in Access_Object) is
   begin
      Pair_Obj.all.Data.Pair.Car := Val;
   end;

   function Cdr (Pair_Obj : Access_Object) return Access_Object is
   begin
      return Pair_Obj.all.Data.Pair.Cdr;
   end;

   procedure Set_Cdr (Pair_Obj : in out Access_Object;
                      Val : in Access_Object) is
   begin
      Pair_Obj.all.Data.Pair.Cdr := Val;
   end;

   -- Thank the RMS for keyboard macros... How else would I have made all of
   --  these car/cdr combos??
   function caar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(obj));
   end;

   function cadr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(obj));
   end;

   function cdar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(obj));
   end;

   function cddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(obj));
   end;

   function caaar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(car(obj)));
   end;

   function caadr(Obj : Access_Object) return Access_Object is
   begin
      return car(car(cdr(obj)));
   end;

   function cadar(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(car(obj)));
   end;

   function caddr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(cdr(obj)));
   end;

   function cdaar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(car(obj)));
   end;

   function cdadr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(cdr(obj)));
   end;

   function cddar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(car(obj)));
   end;

   function cdddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(cdr(obj)));
   end;

   function caaaar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(car(car(obj))));
   end;

   function caaadr(Obj : Access_Object) return Access_Object is
   begin
      return car(car(car(cdr(obj))));
   end;

   function caadar(Obj : Access_Object) return Access_Object is
   begin
      return car(car(cdr(car(obj))));
   end;

   function caaddr(Obj : Access_Object) return Access_Object is
   begin
      return car(car(cdr(cdr(obj))));
   end;

   function cadaar(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(car(car(obj))));
   end;

   function cadadr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(car(cdr(obj))));
   end;

   function caddar(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(cdr(car(obj))));
   end;

   function cadddr(Obj : Access_Object) return Access_Object is
   begin
      return car(cdr(cdr(cdr(obj))));
   end;

   function cdaaar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(car(car(obj))));
   end;

   function cdaadr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(car(cdr(obj))));
   end;

   function cdadar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(cdr(car(obj))));
   end;

   function cdaddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(car(cdr(cdr(obj))));
   end;

   function cddaar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(car(car(obj))));
   end;

   function cddadr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(car(cdr(obj))));
   end;

   function cdddar(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(cdr(car(obj))));
   end;

   function cddddr(Obj : Access_Object) return Access_Object is
   begin
      return cdr(cdr(cdr(cdr(obj))));
   end;

   function Make_Symbol (Value : Unbounded_String) return Access_Object is
      Obj : Access_Object;
      Element : Access_Object;

      function Same_Str(One, Two : Unbounded_String) return Boolean is
      begin
         return Length(One) = Length(Two) and then One = Two;
      end;

   begin
      -- Search for the symbol in the symbol table.
      Element := Symbol_Table;
      while Is_The_Empty_List(Element) /= True loop
         if Same_Str(Car(Element).all.Data.Symbol, Value) then
            return Car(Element);
         end if;
         Element := Cdr(Element);
      end loop;

      Obj := Allowc_Object;
      Obj.all.O_Type := Symbol;
      Obj.all.Data.Symbol := Value;
      Symbol_Table := Cons(Obj, Symbol_Table);
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

   function Make_Char (C : Character) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Allowc_Object;
      Obj.all.O_Type := Char;
      Obj.all.Data.Char := C;
      return Obj;
   end;

   function Make_String (Str : Unbounded_String) return Access_Object is
      Obj : Access_Object;
   begin
      Obj := Allowc_Object;
      Obj.all.O_Type := Strng;
      Obj.all.Data.Strng := Str;
      return Obj;
   end;


   procedure Init is
   begin
      The_Empty_List := Allowc_Object;
      The_Empty_List.all.O_Type := Empty_List;

      False_Singleton := Allowc_Object;
      False_Singleton.all.O_Type := Bool;
      False_Singleton.all.Data.Bool := False;

      True_Singleton := Allowc_Object;
      True_Singleton.all.O_Type := Bool;
      True_Singleton.all.Data.Bool := True;

      Symbol_Table := The_Empty_List;
   end;

   -- READ ----------------------------------------------------------------

   procedure Read (Str : in out Unbounded_String;
                   Obj : out Access_Object) is

      procedure Read_From_Index (Str : in out Unbounded_String;
                                 I : in out Integer;
                                 Obj : in out Access_Object);

      I : Integer := 1;

      procedure Eat_Whitespace (Str : in Unbounded_String;
                                I : in out Integer) is
      begin
         loop
            exit when Element(Str, I) /= ' ';
            I := I + 1;
         end loop;
      end;

      function Is_Delimiter (C : Character) return Boolean is
      begin
         return C = ' ' or else C = '('
           or else C = ')' or else C = '"';
      end;

      function Is_Space (C : Character) return Boolean is
      begin
         return C = ' ';
      end;

      function Is_Initial (C : Character) return Boolean is
      begin
         return Is_Alphanumeric(C) or else C = '*' or else
           C = '/' or else C = '>' or else
           C = '<' or else C = '=' or else
           C = '?' or else C = '!';
      end;

      procedure Read_String (Str : in out Unbounded_String;
                             Obj_Str : in out Unbounded_String;
                             I : in out Integer) is
      begin
         loop
            begin
               exit when Element(Str, I) = '"';

               if Element(Str, I) = '\' then
                  I := I + 1;
                  case Element(Str, I) is
                     when 'n' =>
                        Append(Obj_Str, Character'Val(10));
                     when '"' =>
                        Append(Obj_Str, '"');
                     when '\' =>
                        Append(Obj_Str, '\');
                     when others =>
                        Stderr("Unkown character escape.");
                        raise Constraint_Error;
                  end case;
               else
                  Append(Obj_Str, Slice(Str, I, I));
               end if;
               I := I + 1;

            exception
               when Ada.Strings.Index_Error =>
                  Append(Obj_Str, Character'Val(10));
                  Str := Get_Line;
                  I := 1;
            end;
         end loop;
         I := I + 1;
      end;

      procedure Read_Integer (Str : in out Unbounded_String;
                              I : in out Integer;
                              Obj : in out Access_Object) is
         Sign : Integer := 1;
         Num : Integer := 0;
      begin
         if Element(Str, I) = '-' then
            Sign := -1;
            I := I + 1;
         end if;

         while Length(Str) >= I and then Is_Digit(Element(Str, I)) loop
            Num := (Num * 10);
            Num := Num + (Character'Pos(Element(Str, I)) - Character'Pos('0'));
            I := I + 1;
         end loop;
         Num := Num * Sign;

         if I = Length(Str) + 1 or else Is_Delimiter(Element(Str, I)) then
            Obj := Make_Integer(Num);
            return;
         else
            Stderr("Number not followed by a delimiter.");
            raise Constraint_Error;
         end if;
      end;

      procedure Read_Character (Str : in out Unbounded_String;
                                I : in out Integer;
                                Obj : in out Access_Object) is
      begin
         -- Check for "#\space" and "#\newline"
         begin
            if Element(Str, I) = 's' then
               if Slice(Str, I, I + 4) = "space" then
                  Obj := Make_Char(' ');
                  I := I + 5;
                  return;
               end if;
            elsif Element(Str, I) = 'n' then
               if Slice(Str, I, I + 6) = "newline" then
                  Obj := Make_Char(Character'Val(10));
                  I := I + 7;
                  return;
               end if;
            end if;
         exception
            when Ada.Strings.Index_Error =>
               null;
         end;

         -- If the index fails, that means a newline was entered since Ada
         -- won't keep the last \n.
         begin
            Obj := Make_Char(Element(Str, I));
            I := I + 1;
            return;
         exception
            when Ada.Strings.Index_Error =>
               Obj := Make_Char(Character'Val(10));
               return;
         end;
      end;

      procedure Read_Pair (Str : in out Unbounded_String;
                           I : in out Integer;
                           Obj : in out Access_Object) is
         Car_Obj : Access_Object;
         Cdr_Obj : Access_Object;
      begin
         Eat_Whitespace(Str, I);

         if Element(Str, I) = ')' then
            Obj := The_Empty_List;
            I := I + 1;
            return;
         end if;

         Read_From_Index(Str, I, Car_Obj);
         Eat_Whitespace(Str, I);

         loop
            begin
               if Element(Str, I) = '.' then
                  -- Improper list
                  I := I + 1;
                  Eat_Whitespace(Str, I);
                  Read_From_Index(Str, I, Cdr_Obj);

                  Eat_Whitespace(Str, I);
                  if Element(Str, I) /= ')' then
                     Stderr("No trailing paren after pair.");
                     raise Constraint_Error;
                  end if;

                  Obj := Cons(Car_Obj, Cdr_Obj);
                  I := I + 1;
                  return;

               else
                  -- Proper list
                  Read_Pair(Str, I, Cdr_Obj);
                  Obj := Cons(Car_Obj, Cdr_Obj);
                  return;
               end if;
            exception
               when Ada.Strings.Index_Error =>
                  Get_Line(Str);
                  I := 1;
            end;
         end loop;
      end;

      procedure Read_From_Index(Str : in out Unbounded_String;
                                I : in out Integer;
                                Obj : in out Access_Object) is
      begin

         while I <= Length(Str) loop
            if Is_Space(Element(Str, I)) then
               -- Continue
               I := I + 1;

            elsif Element(Str, I) = '"' then
               -- Read a String
               I := I + 1;
               declare
                  Obj_Str : Unbounded_String;
               begin
                  Read_String(Str, Obj_Str, I);
                  Obj := Make_String(Obj_Str);
                  return;
               end;

            elsif
              (Is_Initial(Element(Str, I)) and then not Is_Digit(Element(Str, I))) or else
              ((Element(Str, I) = '+' or else Element(Str, I) = '-') and then
                 Is_Delimiter(Element(Str, I + 1)))
            then
               declare
                  Symb_Str : Unbounded_String;
               begin
                  begin
                     while
                       Is_Initial(Element(Str, I)) or else Is_Digit(Element(Str, I))
                       or else Element(Str, I) = '+' or else Element(Str, I) = '-'
                     loop
                        Append(Symb_Str, Element(Str, I));
                        I := I + 1;
                     end loop;
                  exception
                     when Ada.Strings.Index_Error =>
                        -- End of line, no more chars to add to the symbol
                        --  string.
                        null;
                  end;

                  Obj := Make_Symbol(Symb_Str);
                  return;
               end;

            -- Lists
            elsif Element(Str, I) = '(' then
               I := I + 1;
               Read_Pair(Str, I, Obj);
               return;

            elsif Element(Str, I) = '#' then
               I := I + 1;
               if Element(Str, I) = '\' then
                  -- Read a character
                  I := I + 1;
                  Read_Character(Str, I, Obj);
                  return;

               else
                  -- Read a boolean
                  case Element(Str, I) is
                     when 't' => Obj := True_Singleton;
                     when 'f' => Obj := False_Singleton;
                     when others =>
                        Stderr("Unknown boolean literal.");
                        raise Constraint_Error;
                  end case;
                  I := I + 1;
                  return;
               end if;

            elsif Is_Digit(Element(Str, I)) or else Element(Str, I) = '-' then
               -- Read an integer
               Read_Integer(Str, I, Obj);
               return;
            else
               Stderr("Read illegal state.");
               raise Constraint_Error;
            end if;
         end loop;

         Stderr("Uh oh read is returning without setting the Obj");
         return;
      end;

   begin
      Read_From_Index(Str, I, Obj);
   end;
   -- EVAL ----------------------------------------------------------------

   -- Until we have lists and symbols, just echo
   function Eval (Exp : Access_Object) return Access_Object is
   begin
      return Exp;
   end;

   -- PRINT ---------------------------------------------------------------

   procedure Print (Obj : in Access_Object) is

      procedure Print_Pair (Pair_Obj : in Access_Object) is
         Car_Obj : Access_Object := Car(Pair_Obj);
         Cdr_Obj : Access_Object := Cdr(Pair_Obj);
      begin
         Print(Car_Obj);
         if Cdr_Obj.all.O_Type = Pair then
            Put(" ");
            Print_Pair(Cdr_Obj);
            return;
         elsif Cdr_Obj = The_Empty_List then
            return;
         else
            Put(" . ");
            Print(Cdr_Obj);
            return;
         end if;
      end;

   begin
      if Obj = null then
         Stderr("Null object type.");
         raise Constraint_Error;
      end if;

      case Obj.all.O_Type is
         when Int =>
            Put(Obj.all.Data.Int, Width => 0);
         when Bool =>
            if Obj.all.Data.Bool = True then
               Put("#t");
            else
               Put("#f");
            end if;
         when Symbol =>
            Put(Obj.all.Data.Symbol);
         when Char =>
            declare
               Str : Unbounded_String;
            begin
               Append(Str, "#\ ");
               case Obj.all.Data.Char is
                  when ' ' =>
                     Insert(Str, 3, "space");
                  when Character'Val(10) =>
                     Insert(Str, 3, "newline");
                  when others =>
                     Replace_Element(Str, 3, Obj.all.Data.Char);
               end case;
               Put(Str);
            end;
         when Strng =>
            Put('"');
            for I in 1 .. Length(Obj.all.Data.Strng) loop
               if Element(Obj.all.Data.Strng, I) = Character'Val(10) then
                  Put("\n");
               elsif Element(Obj.all.Data.Strng, I) = '"' then
                  Put('\');
                  Put('"');
               elsif Element(Obj.all.Data.Strng, I) = '\' then
                  Put("\\");
               else
                  Put(Element(Obj.all.Data.Strng, I));
               end if;
            end loop;
            Put('"');
         when Empty_List =>
            Put("()");
         when Pair =>
            Put("(");
            Print_Pair(Obj);
            Put(")");
         when others =>
            Stderr("Cannot write unknown data type.");
            raise Constraint_Error;
      end case;
   end;

   Str : Unbounded_String;
   Obj : Access_Object;

begin

   Init;

   -- REPL ----------------------------------------------------------------

   Ada.Text_Io.Put_Line("Welcome to Bootstrap Scheme -- Ada version.");

   loop
      Put("> ");
      Get_Line(Str);
      Read(Str, Obj);
      Print(Eval(Obj));
      New_Line;
   end loop;

end;


-- MUSIC ------------------------------------------------------------------

-- Lifer's Group, Grand Puba, Nightmares On Wax, Binary Star
