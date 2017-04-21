using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HashTableVisualizer
{
    public class ListViewData
    {
        public ListViewData(string id, string name, string count)
        {
            Id = id;
            Name = name;
            Count = count;
        }

        public string Id { get; set; }
        public string Name { get; set; }
        public string Count { get; set; }
    }
}
