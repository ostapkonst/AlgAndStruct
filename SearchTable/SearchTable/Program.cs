using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

/*
 ТЕМА: Самоорганизующиеся таблицы
 ЦЕЛЬ РАБОТЫ: исследовать и изучить оптимизирующие методы поиска с помощью самоорганизующихся таблиц.
 */


class Table : ICloneable
{
    public enum BalancingType
    {
        MovingToTheBeginning, // Метод перемещения в начало
        Transposition // Метод транспозиции
    };

    private List<TableCell> Cells;
    public int IntervalWidth { get; private set; }
    public BalancingType Balance { get; set; }

    public Table(int count, int range, BalancingType balance)
    {
        Cells = new List<TableCell>();
        Balance = balance;
        var rnd = new Random();

        IntervalWidth = 0;
        for (int i = 0; i < count; i++)
        {
            int randNum = rnd.Next(1, range + 1);
            var val = new TableCell(i, IntervalWidth + 1, IntervalWidth = IntervalWidth + randNum, randNum);
            Cells.Add(val);
        }

        Cells.Sort(new TableCell.ComparerSerialNumbers());
        for (int i = 0; i < count; i++)
            Cells[i].SerialNumber = i;
        Cells.Sort(new TableCell.ComparerKeys());
    }

    // Обмен элементами списка
    private static void Swap<T>(IList<T> list, int indexA, int indexB)
    {
        T tmp = list[indexA];
        list[indexA] = list[indexB];
        list[indexB] = tmp;
    }

    public int DegreeOfBalance()
    {
        int result = 0;
        for (int i = 0; i < Cells.Count(); i++)
            result += Math.Abs(Cells[i].SerialNumber - i);

        return result;
    }

    public TableCell this[int key]
    {
        get
        {
            for (int i = 0; i < Cells.Count(); i++)
                if (Cells[i].StartInterval <= key && key <= Cells[i].EndInterval)
                {
                    var res = Cells[i];
                    switch (Balance)
                    {
                        case BalancingType.MovingToTheBeginning:
                            Cells.Remove(res);
                            Cells.Insert(0, res);
                            break;
                        case BalancingType.Transposition:
                            if (i > 0)
                                Swap(Cells, i - 1, i);
                            break;
                        default:
                            throw new NotImplementedException();
                    }
                    return res;
                }

            return null;
        }
    }

    public override string ToString()
    {
        return string.Join("\n", Cells);
    }

    public object Clone()
    {
        var copyObj = (Table)this.MemberwiseClone();
        copyObj.Cells = new List<TableCell>(this.Cells); // Скопируются только ссылки
        return copyObj;
    }
}


class TableCell
{
    public int Key { get; private set; }
    public int StartInterval { get; private set; }
    public int EndInterval { get; private set; }
    public int SerialNumber { get; set; }

    public TableCell(int key, int startInterval, int endInterval, int serialNumber)
    {
        Key = key;
        StartInterval = startInterval;
        EndInterval = endInterval;
        SerialNumber = serialNumber;
    }

    public override string ToString()
    {
        return string.Format("Key = {0}, Start = {1}, End = {2}, Serial = {3}", Key, StartInterval, EndInterval, SerialNumber);
    }

    // Сортировка по убыванию размера интервала
    public class ComparerSerialNumbers : IComparer<TableCell>
    {
        public int Compare(TableCell x, TableCell y)
        {
            return y.SerialNumber.CompareTo(x.SerialNumber);
        }
    }

    // Сортировка по возрастанию ключа
    public class ComparerKeys : IComparer<TableCell>
    {
        public int Compare(TableCell x, TableCell y)
        {
            return x.Key.CompareTo(y.Key);
        }
    }
}

namespace SearchTable
{
    class Program
    {
        static void PrintMetrics(Table table, int[] countOfKeys, bool printTable = true)
        {
            var rnd = new Random();

            foreach (var number in countOfKeys)
            {
                var tempTable = (Table)table.Clone();
                for (int i = 0; i < number; i++)
                {
                    int randNum = rnd.Next(1, tempTable.IntervalWidth + 1);
                    var movedCell = tempTable[randNum];
                    if (printTable)
                    {
                        Console.WriteLine("Search: {0}, Find: [{1}]", randNum, movedCell);
                        Console.ReadKey();
                    }
                }

                if (printTable)
                {
                    Console.WriteLine();
                    Console.WriteLine(tempTable);
                    Console.WriteLine("Balance: {0} (for {1} appeals)", tempTable.DegreeOfBalance(), number);
                    Console.WriteLine();
                    Console.ReadKey();
                }
                else
                    Console.WriteLine("Balance: {0} (for {1} appeals)", tempTable.DegreeOfBalance(), number);
            }
        }
        static void Main(string[] args)
        {
            const int count = 25; // Количество ключей
            const int range = 100; // Диапазон генерируемых чисел на каждом шаге

            // Последовательность количества ключей для поиска
            int[] countOfKeys = { count / 2, count, 2 * count, 5 * count, 10 * count };


            var tableMTTB = new Table(count, range, Table.BalancingType.MovingToTheBeginning);
            var tableT = ((Table)tableMTTB.Clone());
            tableT.Balance = Table.BalancingType.Transposition;

            Console.WriteLine("Moving to the beginning method");
            PrintMetrics(tableMTTB, countOfKeys);

            Console.WriteLine("Transposition method");
            PrintMetrics(tableT, countOfKeys);
        }
    }
}
