using System;
using System.Collections.Generic;
using System.IO;
namespace Компилятор
{
    struct TextPosition//Структура Позиция в файле
    {
        public uint lineNumber; // номер строки
        public byte charNumber; // номер позиции в строке

        public TextPosition(uint ln = 0, byte c = 0) //Конструктор
        {
            lineNumber = ln; //линия 
            charNumber = c; //столбик 
        }
    }
    struct Err // стуктура ошибок 
    {
        public TextPosition errorPosition;// позиция ошибки
        public byte errorCode; // код ошибки

        public Err(TextPosition errorPosition, byte errorCode) //конструктор 
        {
            this.errorPosition = errorPosition;
            this.errorCode = errorCode;
        }
    }
    class InputOutput
    {
        const byte ERRMAX = 9; // макс. число ошибок в строке, которые увидит пользователь
        public static char Ch = ' '; // текущий символ
        public static TextPosition positionNow = new TextPosition(); // указатель на текущую позицию
        static string line = ""; // считанная строка
        public static byte lastInLine = 1;// последний символ в строке
        public static List<Err> err;// список ошибок
        public static StreamReader File { get; set; }// файл для чтения
        public static uint errCount = 0;// количество ошибок
        public static bool flag = true;// false при окончании чтения
        public static bool startstring, temp = false;
        public static bool fornext = false;

        /// <summary>
        /// Метод перехода к следующему символу исходной программы.
        /// Используется в процессе лексического анализа. Обновляет текущую позицию и символ.
        /// </summary>
        static public void NextCh()
        {
            // Если текущий символ — последний в строке
            if (positionNow.charNumber == lastInLine)
            {
                // Если были ошибки в предыдущей строке — вывести их
                if (errCount > 0)
                {
                    ListErrors(); // Вывод накопленных ошибок на экран
                }

                // Если флаг чтения активен, читаем новую строку из файла
                if (flag)
                    ReadNextLine();

                // Переходим на следующую строку (увеличиваем номер строки)
                positionNow.lineNumber = positionNow.lineNumber + 1;

                // Сбрасываем позицию по символу (начало строки)
                positionNow.charNumber = 0;

                // Флаг начала строки устанавливается в true
                startstring = true;

                // Сброс временного флага — используется в лексическом анализе
                temp = false;
            }
            else
            {
                // Если встретили непустой символ после пробела и это потенциальное начало нового слова
                if (Ch != ' ' && temp && LexicalAnalyzer.endworld)
                {
                    startstring = false; // Убираем флаг начала строки, мы внутри слова
                }

                // Первый непустой символ, временно помечаем как начало слова
                if (Ch != ' ' && !temp)
                {
                    temp = true;
                }

                // Переходим на следующий символ в строке
                ++positionNow.charNumber;
            }

            // Если текущая строка не пуста — обновляем Ch на новый символ
            if (line.Length != 0)
                Ch = line[positionNow.charNumber]; // Получаем следующий символ по текущей позиции
            //Console.WriteLine($"Символ: {Ch}, Строка: {positionNow.lineNumber}, Позиция: {positionNow.charNumber}");

        }

        private static void ReadNextLine()  // метод чтения следующей строки 
        {
            if (!(File.EndOfStream)) // если не конец файла
            {
                line = File.ReadLine(); //Считываем строку файла
                Console.WriteLine(line); //Выводим строку

                err = new List<Err>(); // создание листа с ошибками 
                if (line.Length != 0) //Если длина строки не равна 0
                {
                    lastInLine = Convert.ToByte(line.Length - 1); //Высчитваем позицию конца строки
                }
            }
            else
            {
                End();//завершение программы
            }
        }
        public static void End() //Завершение программы
        {
            flag = false; //окончание чтения
            if (!SyntaxAnalis.endp && SyntaxAnalis.begino)
            {
                InputOutput.Error(61, InputOutput.positionNow);
                Console.WriteLine("**" + errCount + "**" + "^ ошибка код 61");
                Console.WriteLine("Ожидалась точка");
            }

            Console.ForegroundColor = (ConsoleColor)10; //покарска сообщения о компиляции 
            if (errCount == 0) //Если ошибок нет 
                Console.ForegroundColor = (ConsoleColor)10;//Цвет текста в консоли зеленый
            else
                Console.ForegroundColor = (ConsoleColor)12;//Иначе красный

            Console.WriteLine($"Компиляция завершена: : ошибок — {errCount}!"); //Вывод кол-ва ошибок 
            Console.ReadLine();
            Environment.Exit(0); //завершение программы 
        }
        static public void ListErrors()
        {
            int pos = 1 - $"{positionNow.lineNumber} ".Length;//вычисление позции ошибки
            string s;
            foreach (Err item in err)
            {
                s = "**";// добавить в итоговую строку **
                if (errCount < 10) s += "0"; // добавить в итоговую строку 0 перед числами 1..9
                s += $"{errCount}**";// добавить в итоговую строку кол-во ошибок
                while (s.Length - 1 < pos + item.errorPosition.charNumber) s += " ";// добавить пробелов до позиции символа с ошибкой
                s += $"^ ошибка код {item.errorCode}";// добавить в итоговую строку указатель и код ошибки
                Console.WriteLine(s);
                string value;
                if (Erorr.codeerror.TryGetValue(item.errorCode, out value))
                {
                    Console.WriteLine("****** " + Erorr.codeerror[item.errorCode]);
                    Console.WriteLine();
                }
            }

        }
        static public void Error(byte errorCode, TextPosition position)
        {
            errCount++; // увеличить кол-во ошибок
            Err e;
            if (errCount <= ERRMAX) // если кол-во ошибок не больше максимального
            {
                e = new Err(position, errorCode);// создать новую ошибку
                err.Add(e); // добавить её в список
            }
        }
    }
}