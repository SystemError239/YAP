using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace Компилятор
{
    class SyntaxAnalis
    {
        private static TextPosition first = new TextPosition();
        private static List<string> variables = new List<string>();
        private static string[] types = { "shortint", "integer", "longint", "byte", "word", "real", "double", "single", "extended", "boolean", "char" };
        private static byte symbol;
        private static List<string> procedure_v = new List<string>();
        private static List<string> procedure_n = new List<string>();
        private static int q = 0;
        public static bool be_end_flag, begino, endp = false;
        private static LexicalAnalyzer lex = new LexicalAnalyzer();
        private static int i = 0;
        private static int beginNesting = 0;
        
        private static List<string> procedureParams = new List<string>();

        private static bool Correct(byte symbol_Correct, TextPosition TextPos)
        {
            if (symbol == symbol_Correct)
            {
                return true;
            }
            else
            {
                InputOutput.Error(symbol_Correct, TextPos);
                return false;
            }
        }

        public static void chechType()
        {
            if (symbol == LexicalAnalyzer.ident)
            {
                string type = lex.ident_name;
                bool q = false;
                for (int i = 0; i < types.Length; i++)
                {
                    if (types[i] == type)
                    {
                        q = true;
                    }
                }
                if (q == false)
                {
                    if (variables.Count != 0)
                    {
                        variables.RemoveAt(variables.Count - 1);
                    }
                    InputOutput.Error(94, InputOutput.positionNow);
                }
            }
        }

        public static void SyntaxAlanyzer()
        {
            // Если текущий символ не begin — считываем следующий
            if (symbol != LexicalAnalyzer.beginsy)
            {
                symbol = lex.NextSym();
            }

            switch (symbol)
            {
                case LexicalAnalyzer.ifsy:
                    IfStatement();
                    break;

                case LexicalAnalyzer.programsy:
                    Program();
                    break;

                case LexicalAnalyzer.procedurensy:
                    Procedure();
                    break;

                case LexicalAnalyzer.varsy:
                    if (!be_end_flag)
                    {
                        Varvar();
                    }
                    break;

                case LexicalAnalyzer.beginsy:
                    begino = true;
                    CompoundStatement();
                    break;

                case LexicalAnalyzer.ident:
                    if (InputOutput.startstring)
                    {
                        Ident();
                    }
                    else
                    {
                        symbol = lex.NextSym();
                    }
                    break;
                case LexicalAnalyzer.casesy:
                    CaseStatement();
                    break;

                case LexicalAnalyzer.endsy:
                    // Если встречаем end вне CompoundStatement — это несоответствие парности begin/end
                    InputOutput.Error(113, InputOutput.positionNow);
                    symbol = lex.NextSym();
                    break;

                case LexicalAnalyzer.semicolon:
                    symbol = lex.NextSym();
                    break;

                default:
                    symbol = lex.NextSym();
                    break;
            }
        }




        public static void Varvar()
        {
            if (symbol == LexicalAnalyzer.varsy)
            {
                symbol = lex.NextSym();
                do
                {
                    Vardeclaration();
                    symbol = lex.NextSym();
                    Correct(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                    symbol = lex.NextSym();
                }
                while (symbol == LexicalAnalyzer.ident || symbol == LexicalAnalyzer.varsy);

                SyntaxAlanyzer();
            }
        }

        public static void Vardeclaration()
        {
            Correct(LexicalAnalyzer.ident, InputOutput.positionNow);
            variables.Add(lex.ident_name);
            symbol = lex.NextSym();
            while (symbol == LexicalAnalyzer.comma)
            {
                symbol = lex.NextSym();
                Correct(LexicalAnalyzer.ident, InputOutput.positionNow);
                variables.Add(lex.ident_name);
                symbol = lex.NextSym();
            }
            Correct(LexicalAnalyzer.colon, InputOutput.positionNow);
            symbol = lex.NextSym();
            if (symbol == LexicalAnalyzer.arraysy)
                Array();
            else
                chechType();
        }

        public static void Begin()
        {
            Correct(LexicalAnalyzer.beginsy, InputOutput.positionNow);
            symbol = lex.NextSym();
            SyntaxAlanyzer();
        }

        public static void End()
        {
            if (beginNesting > 0)
            {
                Correct(LexicalAnalyzer.endsy, InputOutput.positionNow);
                symbol = lex.NextSym();
                beginNesting--;
                if (beginNesting == 0)
                {
                    begino = false;
                    endp = true;
                }
            }
            else
            {
                InputOutput.Error(113, InputOutput.positionNow);
            }
        }

        public static void Ident(bool insideStatement = false)
        {
            if (procedure_n.Any(str => str == lex.ident_name))
            {
                symbol = lex.NextSym();
                Correct(LexicalAnalyzer.leftpar, InputOutput.positionNow);
                while (procedure_v[q] != "0")
                {
                    q++;
                }
                while (procedure_v[i] != "0")
                {
                    symbol = lex.NextSym();
                    if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.ident || symbol == LexicalAnalyzer.floatc)
                    {
                        if (lex.ident_name == procedure_v[i] || symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.floatc)
                        {
                            symbol = lex.NextSym();
                            if (i != q - 1)
                                Correct(LexicalAnalyzer.comma, InputOutput.positionNow);
                        }
                        i++;
                    }
                }
                Correct(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                symbol = lex.NextSym();
                Correct(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                i++;
                q++;
            }
            else
            {
                byte value = 0;
                if (!Keywords.Kw.TryGetValue(lex.ident_name, out value))
                {
                    bool isParam = procedureParams.Contains(lex.ident_name);
                    bool isVariable = variables.Contains(lex.ident_name);

                    if (InputOutput.startstring || insideStatement)

                    {
                        // Проверяем, объявлена ли переменная или параметр
                        CheckVariableExists(lex.ident_name);

                        // Если ошибка (код 88) не сработал, продолжим
                        symbol = lex.NextSym();

                        // Если это присваивание :=
                        if (symbol == LexicalAnalyzer.assign)
                        {

                            symbol = lex.NextSym();
                            // запись символьного кода в файл
                            StreamWriter writer = new StreamWriter("Sim.txt", true);//открытие для записи 
                            if (symbol != 0 && InputOutput.flag)//если с данным символов не возникло ошибки и файл открыт для чтения 
                                writer.Write(symbol + " ");//добавляем значение в файл
                                                           //переход на новую строку
                            if (InputOutput.positionNow.charNumber == 0 && InputOutput.flag)// если достигли конца и файл доступен для чтения 
                                writer.Write('\n');//переход на новую строку 
                            writer.Close();// закрытие файла
                            // Пропускаем всё до конца выражения
                            while (!IsExpressionEnd(symbol) && symbol != 0)
                            {
                                symbol = lex.NextSym();
                            }
                        }
                        // Иначе — если после идентификатора не :=, мы просто ничего не делаем дальше
                    }

                }
            }
        }

        public static void Array()
        {
            Correct(LexicalAnalyzer.arraysy, InputOutput.positionNow);
            symbol = lex.NextSym();
            Correct(LexicalAnalyzer.lbracket, InputOutput.positionNow);
            symbol = lex.NextSym();
            if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.ident)
            {
                symbol = lex.NextSym();
                if (symbol == LexicalAnalyzer.rbracket)
                {
                    conArray();
                }
                else
                {
                    Correct(LexicalAnalyzer.twopoints, InputOutput.positionNow);
                    symbol = lex.NextSym();
                    if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.ident)
                    {
                        symbol = lex.NextSym();
                        Correct(LexicalAnalyzer.rbracket, InputOutput.positionNow);
                        conArray();
                    }
                    else
                    {
                        InputOutput.Error(1, InputOutput.positionNow);
                    }
                }
            }
            else
            {
                InputOutput.Error(1, InputOutput.positionNow);
            }
        }

        public static void conArray()
        {
            symbol = lex.NextSym();
            Correct(LexicalAnalyzer.ofsy, InputOutput.positionNow);
            symbol = lex.NextSym();
            chechType();
        }

        public static void Program()
        {
            Correct(LexicalAnalyzer.programsy, InputOutput.positionNow);
            symbol = lex.NextSym();
            Correct(LexicalAnalyzer.ident, InputOutput.positionNow);
            symbol = lex.NextSym();
            Correct(LexicalAnalyzer.semicolon, InputOutput.positionNow);
            symbol = lex.NextSym();
        }

        public static void Procedure()
        {
            Correct(LexicalAnalyzer.procedurensy, InputOutput.positionNow);
            symbol = lex.NextSym();
            Correct(LexicalAnalyzer.ident, InputOutput.positionNow);
            procedure_n.Add(lex.ident_name);
            symbol = lex.NextSym();

            procedureParams.Clear();

            if (!(symbol == LexicalAnalyzer.semicolon))
            {
                Correct(LexicalAnalyzer.leftpar, InputOutput.positionNow);
                symbol = lex.NextSym();
                do
                {
                    Correct(LexicalAnalyzer.ident, InputOutput.positionNow);
                    string paramName = lex.ident_name;
                    procedure_v.Add(paramName);
                    procedureParams.Add(paramName);
                    symbol = lex.NextSym();
                    while (symbol == LexicalAnalyzer.comma)
                    {
                        symbol = lex.NextSym();
                        Correct(LexicalAnalyzer.ident, InputOutput.positionNow);
                        procedure_v.Add(lex.ident_name);
                        procedureParams.Add(lex.ident_name);
                        symbol = lex.NextSym();
                    }
                    Correct(LexicalAnalyzer.colon, InputOutput.positionNow);
                    symbol = lex.NextSym();
                    chechType();
                    symbol = lex.NextSym();
                    if (symbol == LexicalAnalyzer.semicolon)
                    {
                        Correct(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                    }
                    else
                    {
                        Correct(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                    }
                    symbol = lex.NextSym();
                }
                while (symbol == LexicalAnalyzer.ident);
                Correct(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                procedure_v.Add("0");
                SyntaxAlanyzer();
            }

            procedureParams.Clear();
        }

        private static void CompoundStatement()
        {
            beginNesting++;
            Correct(LexicalAnalyzer.beginsy, InputOutput.positionNow);
            symbol = lex.NextSym(); // съели begin

            while (InputOutput.flag)
            {
                // 1) Пропускаем ';'
                if (symbol == LexicalAnalyzer.semicolon)
                {
                    symbol = lex.NextSym();
                    continue;
                }

                // 2) Если встретили end — закрываем текущий begin
                if (symbol == LexicalAnalyzer.endsy)
                {
                    // Если нет открытого блока — лишний end
                    if (beginNesting == 0)
                    {
                        InputOutput.Error(113, InputOutput.positionNow); // Ожидался begin
                        symbol = lex.NextSym();
                        return;
                    }

                    Correct(LexicalAnalyzer.endsy, InputOutput.positionNow);
                    symbol = lex.NextSym(); // съели end
                    beginNesting--;

                    // Если мы закрыли последний внешний begin, выходим
                    if (beginNesting == 0)
                    {
                        begino = false;
                        endp = true;
                        return;
                    }
                    continue;
                }

                // 3) Вложенный begin → рекурсивный вызов
                if (symbol == LexicalAnalyzer.beginsy)
                {
                    CompoundStatement();
                    continue;
                }

                // 4) if
                if (symbol == LexicalAnalyzer.ifsy)
                {
                    IfStatement();
                    continue;
                }

                // 5) for
                if (symbol == LexicalAnalyzer.forsy)
                {
                    ForStatement();
                    continue;
                }

                // 6) case
                if (symbol == LexicalAnalyzer.casesy)
                {
                    CaseStatement();
                    continue;
                }

                // 7a) write/writeln/read/readln
                if (symbol == LexicalAnalyzer.writeln ||
                    symbol == LexicalAnalyzer.write ||
                    symbol == LexicalAnalyzer.read ||
                    symbol == LexicalAnalyzer.readln)
                {
                    symbol = lex.NextSym();
                    if (symbol == LexicalAnalyzer.leftpar)
                    {
                        do { symbol = lex.NextSym(); }
                        while (symbol != LexicalAnalyzer.rightpar && symbol != 0);

                        if (symbol == LexicalAnalyzer.rightpar)
                            symbol = lex.NextSym();
                        else
                            InputOutput.Error(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.leftpar, InputOutput.positionNow);
                    }

                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.endsy)
                        {
                            symbol = lex.NextSym();
                        }
                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                    continue;
                }

                // 7b) ident (присваивание или вызов пользоват. процедуры)
                if (symbol == LexicalAnalyzer.ident)
                {
                    Ident(true);
                    if (symbol == LexicalAnalyzer.semicolon)
                    {
                        symbol = lex.NextSym();
                    }
                    else if(symbol == LexicalAnalyzer.endsy)
                    {
                       // symbol = lex.NextSym();
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.endsy)
                        {
                            symbol = lex.NextSym();
                        }
                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                    continue;
                }

                // 8) Всё остальное — общий разбор
                SyntaxAlanyzer();
            }
        }

        public static void IfStatement()
        {
            // 1. Проверяем слово if
            Correct(LexicalAnalyzer.ifsy, InputOutput.positionNow);
            symbol = lex.NextSym();

            // 2. Условие: поддержка and/or и скобок
            ParseBooleanExpression();

            // 3. Сохраняем позицию, где мы ожидаем then
            TextPosition thenExpectedPos = InputOutput.positionNow;

            while (!IsConditionEnd(symbol) && symbol != 0)
            {
                thenExpectedPos = InputOutput.positionNow;
                symbol = lex.NextSym();
            }

            // 4. Проверка then
            if (symbol != LexicalAnalyzer.thensy)
            {
                InputOutput.Error(LexicalAnalyzer.thensy, thenExpectedPos);
                return;
            }

            symbol = lex.NextSym();

            // 5. then-блок
            

            if (symbol == LexicalAnalyzer.beginsy)
            {
                CompoundStatement();
                if (symbol == LexicalAnalyzer.semicolon)
                {
                    TextPosition semicolonPos = InputOutput.positionNow;
                    byte nextSym = lex.PeekNextSym();

                    if (nextSym == LexicalAnalyzer.elsesy)
                    {
                        InputOutput.Error(214, semicolonPos); // Ошибка: ; перед else
                    }

                    symbol = lex.NextSym(); // съедаем ;
                }


            }
            else if (symbol == LexicalAnalyzer.ident)
            {
                Ident();

                TextPosition semicolonPos = InputOutput.positionNow;

                if (symbol == LexicalAnalyzer.semicolon)
                {
                    
                    symbol = lex.NextSym();

                    // Ошибка: ; перед else запрещена
                    if (symbol == LexicalAnalyzer.elsesy)
                    {
                        InputOutput.Error(214, semicolonPos); // Нельзя ставить ; перед else
                    }
                }
                else if (symbol == LexicalAnalyzer.elsesy)
                {
                    // Допустимый случай — нет точки с запятой перед else
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                    while (InputOutput.flag &&
                           symbol != LexicalAnalyzer.semicolon &&
                           symbol != LexicalAnalyzer.endsy &&
                           symbol != LexicalAnalyzer.elsesy)
                    {
                        symbol = lex.NextSym();
                    }

                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.beginsy, InputOutput.positionNow);
                return;
            }

            // 6. else-блок
            if (symbol == LexicalAnalyzer.elsesy)
            {
                symbol = lex.NextSym();

                if (symbol == LexicalAnalyzer.beginsy)
                {
                    CompoundStatement();
                }
                else if (symbol == LexicalAnalyzer.ident)
                {
                    Ident();

                    if (symbol == LexicalAnalyzer.semicolon)
                    {
                        symbol = lex.NextSym();
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.endsy)
                        {
                            symbol = lex.NextSym();
                        }

                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.beginsy, InputOutput.positionNow);
                }
            }
        }



        private static void ParseBooleanExpression()
        {
            ParseSimpleCondition();

            while (symbol == LexicalAnalyzer.andsy || symbol == LexicalAnalyzer.orsy)
            {
                symbol = lex.NextSym();
                ParseSimpleCondition();
            }
        }

        private static void ParseSimpleCondition()
        {
            if (symbol == LexicalAnalyzer.ident)
            {
                CheckVariableExists(lex.ident_name);
                symbol = lex.NextSym();
            }
            else if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.floatc)
            {
                symbol = lex.NextSym();
            }
            else if (lex.ident_name == "true" || lex.ident_name == "false")
            {
                symbol = lex.NextSym();
            }
            else if (symbol == LexicalAnalyzer.leftpar)
            {
                symbol = lex.NextSym();
                ParseBooleanExpression();
                Correct(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                symbol = lex.NextSym();
                return;
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.ident, InputOutput.positionNow);
                return;
            }

            if (symbol == LexicalAnalyzer.equal ||
                symbol == LexicalAnalyzer.later ||
                symbol == LexicalAnalyzer.greater ||
                symbol == LexicalAnalyzer.laterequal ||
                symbol == LexicalAnalyzer.greaterequal ||
                symbol == LexicalAnalyzer.latergreater)
            {
                symbol = lex.NextSym();

                if (symbol == LexicalAnalyzer.ident)
                {
                    CheckVariableExists(lex.ident_name);
                    symbol = lex.NextSym();
                }
                else if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.floatc)
                {
                    symbol = lex.NextSym();
                }
                else if (lex.ident_name == "true" || lex.ident_name == "false")
                {
                    symbol = lex.NextSym();
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.ident, InputOutput.positionNow);
                }
            }
        }




        public static void ForStatement()
        {
            // 1) Слово "for"
            Correct(LexicalAnalyzer.forsy, InputOutput.positionNow);

            symbol = lex.NextSym();

            // 2) Идентификатор-счётчик
            if (symbol == LexicalAnalyzer.ident)
            {
                CheckVariableExists(lex.ident_name);
                symbol = lex.NextSym();
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.ident, InputOutput.positionNow);
                return;
            }

            // 3) ":="
            if (symbol == LexicalAnalyzer.assign)
            {

                symbol = lex.NextSym();
                // запись символьного кода в файл
                StreamWriter writer = new StreamWriter("Sim.txt", true);//открытие для записи 
                if (symbol != 0 && InputOutput.flag)//если с данным символов не возникло ошибки и файл открыт для чтения 
                    writer.Write(symbol + " ");//добавляем значение в файл
                                               //переход на новую строку
                if (InputOutput.positionNow.charNumber == 0 && InputOutput.flag)// если достигли конца и файл доступен для чтения 
                    writer.Write('\n');//переход на новую строку 
                writer.Close();// закрытие файла
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.assign, InputOutput.positionNow);
                return;
            }

            // 4) Начальное значение (константа или идентификатор)
            if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.floatc)
            {
                symbol = lex.NextSym();
            }
            else if (symbol == LexicalAnalyzer.ident)
            {
                CheckVariableExists(lex.ident_name);
                symbol = lex.NextSym();
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.intc, InputOutput.positionNow);
                return;
            }

            // 5) Ключевое слово "to" или "downto"
            if (symbol == LexicalAnalyzer.tosy || symbol == LexicalAnalyzer.downtosy)
            {
                symbol = lex.NextSym();
                // запись символьного кода в файл
                StreamWriter writer = new StreamWriter("Sim.txt", true);//открытие для записи 
                if (symbol != 0 && InputOutput.flag)//если с данным символов не возникло ошибки и файл открыт для чтения 
                    writer.Write(symbol + " ");//добавляем значение в файл
                                               //переход на новую строку
                if (InputOutput.positionNow.charNumber == 0 && InputOutput.flag)// если достигли конца и файл доступен для чтения 
                    writer.Write('\n');//переход на новую строку 
                writer.Close();// закрытие файла
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.tosy, InputOutput.positionNow);
                return;
            }

            // 6) Конечное значение (константа или идентификатор)
            if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.floatc)
            {
                symbol = lex.NextSym();
            }
            else if (symbol == LexicalAnalyzer.ident)
            {
                CheckVariableExists(lex.ident_name);
                symbol = lex.NextSym();
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.intc, InputOutput.positionNow);
                return;
            }

            // 7) "do"
            if (symbol == LexicalAnalyzer.dosy)
            {
                symbol = lex.NextSym();
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.dosy, InputOutput.positionNow);
                return;
            }

            // 8) Тело цикла: составной оператор или одиночный
            if (symbol == LexicalAnalyzer.beginsy)
            {
                // begin … end
                CompoundStatement();

                // После CompoundStatement() может идти точка с запятой
                if (symbol == LexicalAnalyzer.semicolon)
                {
                    symbol = lex.NextSym();
                }
            }
            else if (symbol == LexicalAnalyzer.writeln ||
                     symbol == LexicalAnalyzer.write ||
                     symbol == LexicalAnalyzer.read ||
                     symbol == LexicalAnalyzer.readln)
            {
                // Встроенная процедура (writeln, write, read, readln)
                symbol = lex.NextSym(); // съели имя процедуры

                // Ожидаем '('
                if (symbol == LexicalAnalyzer.leftpar)
                {
                    // Пропускаем до ')'
                    do
                    {
                        symbol = lex.NextSym();
                    }
                    while (symbol != LexicalAnalyzer.rightpar && symbol != 0);

                    if (symbol == LexicalAnalyzer.rightpar)
                        symbol = lex.NextSym();
                    else
                        InputOutput.Error(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.leftpar, InputOutput.positionNow);
                }

                // После ')' ожидаем ';'
                if (symbol == LexicalAnalyzer.semicolon)
                {
                    symbol = lex.NextSym();
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                    while (InputOutput.flag &&
                           symbol != LexicalAnalyzer.semicolon &&
                           symbol != LexicalAnalyzer.endsy)
                    {
                        symbol = lex.NextSym();
                    }
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
            }
            else if (symbol == LexicalAnalyzer.ident)
            {
                // Присваивание или вызов пользовательской процедуры
                Ident();
                if (symbol == LexicalAnalyzer.semicolon)
                {
                    symbol = lex.NextSym();
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                    while (InputOutput.flag &&
                           symbol != LexicalAnalyzer.semicolon &&
                           symbol != LexicalAnalyzer.endsy)
                    {
                        symbol = lex.NextSym();
                    }
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
            }
            else
            {
                // Ни begin, ни встроенная процедура, ни идентификатор
                InputOutput.Error(LexicalAnalyzer.beginsy, InputOutput.positionNow);
            }
        }

        private static void CheckVariableExists(string varName)
        {
            if (!variables.Contains(varName) && !procedureParams.Contains(varName))
            {
                InputOutput.Error(88, InputOutput.positionNow);
            }
        }

        private static bool IsConditionEnd(byte sym)
        {
            return sym == LexicalAnalyzer.thensy ||
                   sym == LexicalAnalyzer.semicolon ||
                   sym == LexicalAnalyzer.endsy;
        }

        private static bool IsExpressionEnd(byte sym)
        {
            return sym == LexicalAnalyzer.semicolon ||
                   sym == LexicalAnalyzer.endsy ||
                   sym == LexicalAnalyzer.elsesy ||
                   sym == LexicalAnalyzer.thensy;
        }

        public static void CaseStatement()
        {
            // 1. Проверяем слово case и съедаем его
            Correct(LexicalAnalyzer.casesy, InputOutput.positionNow);
            symbol = lex.NextSym();

            // 2. Разбираем выражение после case: идентификатор или целая константа
            if (symbol == LexicalAnalyzer.ident)
            {
                CheckVariableExists(lex.ident_name);
                symbol = lex.NextSym();
            }
            else if (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.floatc)
            {
                symbol = lex.NextSym();
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.ident, InputOutput.positionNow);
                return;
            }

            // 3. Ожидаем "of"
            if (symbol == LexicalAnalyzer.ofsy)
            {
                symbol = lex.NextSym();
            }
            else
            {
                InputOutput.Error(LexicalAnalyzer.ofsy, InputOutput.positionNow);
                return;
            }

            // 4. Разбираем все ветки метки : тело
            while (symbol == LexicalAnalyzer.intc || symbol == LexicalAnalyzer.ident)
            {
                // 4.1. Список меток (включая диапазоны intc..intc и/или списки через запятую)
                bool expectLabel = true;
                while (expectLabel)
                {
                    // (a) Диапазон: intc .. intc
                    if (symbol == LexicalAnalyzer.intc)
                    {
                        // запись символьного кода в файл
                        StreamWriter writer = new StreamWriter("Sim.txt", true);//открытие для записи 
                        if (symbol != 0 && InputOutput.flag)//если с данным символов не возникло ошибки и файл открыт для чтения 
                            writer.Write(symbol + " ");//добавляем значение в файл
                                                       //переход на новую строку
                        if (InputOutput.positionNow.charNumber == 0 && InputOutput.flag)// если достигли конца и файл доступен для чтения 
                            writer.Write('\n');//переход на новую строку 
                        writer.Close();// закрытие файла
                        symbol = lex.NextSym();
                        if (symbol == LexicalAnalyzer.twopoints)
                        {
                            symbol = lex.NextSym();
                            if (symbol == LexicalAnalyzer.intc)
                            {
                                symbol = lex.NextSym();
                            }
                            else
                            {
                                InputOutput.Error(LexicalAnalyzer.intc, InputOutput.positionNow);
                            }
                        }
                    }
                    // (b) Одиночная метка: идентификатор
                    else if (symbol == LexicalAnalyzer.ident)
                    {
                        CheckVariableExists(lex.ident_name);
                        // запись символьного кода в файл
                        StreamWriter writer = new StreamWriter("Sim.txt", true);//открытие для записи 
                        if (symbol != 0 && InputOutput.flag)//если с данным символов не возникло ошибки и файл открыт для чтения 
                            writer.Write(symbol + " ");//добавляем значение в файл
                                                       //переход на новую строку
                        if (InputOutput.positionNow.charNumber == 0 && InputOutput.flag)// если достигли конца и файл доступен для чтения 
                            writer.Write('\n');//переход на новую строку 
                        writer.Close();// закрытие файла
                        symbol = lex.NextSym();
                    }
                    else
                    {
                        expectLabel = false;
                        break;
                    }

                    // (c) Если после метки стоит запятая — едим её и продолжаем разбирать метки
                    if (symbol == LexicalAnalyzer.comma)
                    {
                        // запись символьного кода в файл
                        StreamWriter writer = new StreamWriter("Sim.txt", true);//открытие для записи 
                        if (symbol != 0 && InputOutput.flag)//если с данным символов не возникло ошибки и файл открыт для чтения 
                            writer.Write(symbol + " ");//добавляем значение в файл
                                                       //переход на новую строку
                        if (InputOutput.positionNow.charNumber == 0 && InputOutput.flag)// если достигли конца и файл доступен для чтения 
                            writer.Write('\n');//переход на новую строку 
                        writer.Close();// закрытие файла
                        symbol = lex.NextSym();
                        continue;
                    }
                    else
                    {
                        expectLabel = false;
                    }
                }

                // 4.2. Ожидаем двоеточие :
                if (symbol == LexicalAnalyzer.colon)
                {
                    symbol = lex.NextSym();
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.colon, InputOutput.positionNow);
                    // Пропускаем до ';' или 'else' или 'end'
                    while (InputOutput.flag &&
                           symbol != LexicalAnalyzer.semicolon &&
                           symbol != LexicalAnalyzer.elsesy &&
                           symbol != LexicalAnalyzer.endsy)
                    {
                        symbol = lex.NextSym();
                    }
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                    continue;
                }

                // 4.3. Тело ветки:
                if (symbol == LexicalAnalyzer.beginsy)
                {
                    // a) Составной оператор: begin…end
                    CompoundStatement();
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
                else if (symbol == LexicalAnalyzer.writeln ||
                         symbol == LexicalAnalyzer.write ||
                         symbol == LexicalAnalyzer.read ||
                         symbol == LexicalAnalyzer.readln)
                {
                    // b) Встроенная процедура
                    symbol = lex.NextSym(); // съедаем writeln/write/read/readln

                    if (symbol == LexicalAnalyzer.leftpar)
                    {
                        do { symbol = lex.NextSym(); }
                        while (symbol != LexicalAnalyzer.rightpar && symbol != 0);

                        if (symbol == LexicalAnalyzer.rightpar)
                            symbol = lex.NextSym();
                        else
                            InputOutput.Error(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.leftpar, InputOutput.positionNow);
                    }

                    if (symbol == LexicalAnalyzer.semicolon)
                    {
                        symbol = lex.NextSym();
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.endsy &&
                               symbol != LexicalAnalyzer.elsesy)
                        {
                            symbol = lex.NextSym();
                        }
                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                }
                else if (symbol == LexicalAnalyzer.ident)
                {
                    // c) Простой оператор-пользовательская процедура или присваивание
                    Ident(true);
                    if (symbol == LexicalAnalyzer.semicolon)
                    {
                        symbol = lex.NextSym();
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.elsesy &&
                               symbol != LexicalAnalyzer.endsy)
                        {
                            symbol = lex.NextSym();
                        }
                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                }
                else
                {
                    // Неожиданный токен в теле ветки
                    InputOutput.Error(LexicalAnalyzer.ident, InputOutput.positionNow);
                    while (InputOutput.flag &&
                           symbol != LexicalAnalyzer.semicolon &&
                           symbol != LexicalAnalyzer.elsesy &&
                           symbol != LexicalAnalyzer.endsy)
                    {
                        symbol = lex.NextSym();
                    }
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
            }

            // 5. Ветвь else (если есть)
            if (symbol == LexicalAnalyzer.elsesy)
            {
                symbol = lex.NextSym();

                if (symbol == LexicalAnalyzer.beginsy)
                {
                    CompoundStatement();
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
                else if (symbol == LexicalAnalyzer.writeln ||
                         symbol == LexicalAnalyzer.write ||
                         symbol == LexicalAnalyzer.read ||
                         symbol == LexicalAnalyzer.readln)
                {
                    symbol = lex.NextSym();
                    if (symbol == LexicalAnalyzer.leftpar)
                    {
                        do { symbol = lex.NextSym(); }
                        while (symbol != LexicalAnalyzer.rightpar && symbol != 0);
                        if (symbol == LexicalAnalyzer.rightpar)
                            symbol = lex.NextSym();
                        else
                            InputOutput.Error(LexicalAnalyzer.rightpar, InputOutput.positionNow);
                    }
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.leftpar, InputOutput.positionNow);
                    }
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.endsy)
                        {
                            symbol = lex.NextSym();
                        }
                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                }
                else if (symbol == LexicalAnalyzer.ident)
                {
                    Ident(true);
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                    else
                    {
                        InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                        while (InputOutput.flag &&
                               symbol != LexicalAnalyzer.semicolon &&
                               symbol != LexicalAnalyzer.endsy)
                        {
                            symbol = lex.NextSym();
                        }
                        if (symbol == LexicalAnalyzer.semicolon)
                            symbol = lex.NextSym();
                    }
                }
                else
                {
                    InputOutput.Error(LexicalAnalyzer.ident, InputOutput.positionNow);
                    while (InputOutput.flag &&
                           symbol != LexicalAnalyzer.semicolon &&
                           symbol != LexicalAnalyzer.endsy)
                    {
                        symbol = lex.NextSym();
                    }
                    if (symbol == LexicalAnalyzer.semicolon)
                        symbol = lex.NextSym();
                }
            }

            // === Закрытие конструкции case ===
            if (symbol == LexicalAnalyzer.endsy)
            {
                symbol = lex.NextSym(); // съедаем end

                // Теперь ОЖИДАЕМ ; (как end;)
                if (symbol == LexicalAnalyzer.semicolon)
                {
                    symbol = lex.NextSym(); // всё в порядке
                }
                else
                {
                    // end без точки с запятой
                    InputOutput.Error(LexicalAnalyzer.semicolon, InputOutput.positionNow);
                }

                return;
            }
            else
            {
                // Мы дошли до конца case, а "end" не встретили
                InputOutput.Error(LexicalAnalyzer.endsy, InputOutput.positionNow); // ошибка "Ожидался end"

                // Пропускаем всё до точки с запятой или точки
                while (InputOutput.flag &&
                       symbol != LexicalAnalyzer.point &&
                       symbol != LexicalAnalyzer.semicolon &&
                       symbol != LexicalAnalyzer.endsy)
                {
                    symbol = lex.NextSym();
                }

                // Если всё же дошли до end позже, съедим его (чтобы не мешал)
                if (symbol == LexicalAnalyzer.endsy)
                    symbol = lex.NextSym();
            }

        }
    }
}