using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Компилятор
{
    class Program
    {
        static void Main()
       {
            StreamWriter writer = new StreamWriter("Sim.txt");//Открытие файла для записи для того чтобы его очистить
            writer.Close();//Закрытие файла
                           //Может быть ошибка
            StreamWriter W = new StreamWriter("3.txt", true); //открытие для записи, удаляет старый файл 
            W.Write(" "); //добавляем значение в файл
            W.Close(); // закрытие
            InputOutput.File = new StreamReader ("3.txt");//Открытие основного файла для чтения програмного кода PASCAL 
            LexicalAnalyzer l = new LexicalAnalyzer();
            while (!(InputOutput.File.EndOfStream))//Пока не конец файла
            {
                SyntaxAnalis.SyntaxAlanyzer();
            }
            SyntaxAnalis.SyntaxAlanyzer();
            InputOutput.End();//Завершение программы
        }
    }
}
