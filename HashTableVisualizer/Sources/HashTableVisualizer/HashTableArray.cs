using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HashTableVisualizer
{
    public class HashTableArray<TKey, TValue> where TValue : class
    {
        HashTableArrayNode<TKey, TValue>[] _array;

        public HashTableArray(int capacity)
        {
            _array = new HashTableArrayNode<TKey, TValue>[capacity];
        }

        private int GetIndex(TKey key)
        {
            return Math.Abs(key.GetHashCode() % Capacity);
        }

        public int Capacity
        {
            get
            {
                return _array.Length;
            }
        }

        public void Add(TKey key, TValue value)
        {
            int index = GetIndex(key);
            HashTableArrayNode<TKey, TValue> nodes = _array[index];

            if (nodes == null)
            {
                nodes = new HashTableArrayNode<TKey, TValue>();
                _array[index] = nodes;
            }

            nodes.Add(key, value);
        }

        public bool Remove(TKey key)
        {
            int index = GetIndex(key);
            HashTableArrayNode<TKey, TValue> nodes = _array[index];
            return nodes?.Remove(key) == true;
        }

        public TValue Contains(TKey key)
        {
            int index = GetIndex(key);
            HashTableArrayNode<TKey, TValue> nodes = _array[index];

            TValue value = null;
            nodes?.TryGetValue(key, out value);
            return value;
        }

        public IEnumerable<HashTableNodePair<TKey, TValue>> Items
        {
            get
            {
                foreach (HashTableArrayNode<TKey, TValue> node in _array.Where(node => node != null))
                    foreach (HashTableNodePair<TKey, TValue> pair in node.Items)
                        yield return pair;
            }
        }
    }
}
