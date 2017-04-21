using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HashTableVisualizer
{
    public class HashTable<TKey, TValue> where TValue : class
    {
        // Если заполнение массива первышает 75% - массив увеличивается.     
        const double _fillFactor = 0.75;

        int _maxItemsAtCurrentSize;
        int _count;

        HashTableArray<TKey, TValue> _array;

        public HashTable() : this(1000)
        {
        }

        public HashTable(int initialCapacity)
        {
            if (initialCapacity < 1)
                throw new ArgumentOutOfRangeException("Заданный размер меньше 1!");

            _array = new HashTableArray<TKey, TValue>(initialCapacity);
            _maxItemsAtCurrentSize = (int)(initialCapacity * _fillFactor) + 1;

        }

        public void Add(TKey key, TValue value)
        {
            if (_count >= _maxItemsAtCurrentSize)
            {
                HashTableArray<TKey, TValue> largerArray = new HashTableArray<TKey, TValue>(_array.Capacity * 2);

                foreach (HashTableNodePair<TKey, TValue> node in _array.Items)
                    largerArray.Add(node.Key, node.Value);

                _array = largerArray;
                _maxItemsAtCurrentSize = (int)(_array.Capacity * _fillFactor) + 1;
            }

            _array.Add(key, value);
            _count++;
        }

        public bool Remove(TKey key)
        {
            if (_array.Remove(key))
            {
                _count--;
                return true;
            }

            return false;
        }

        public TValue ContainsKey(TKey key)
        {
            return _array.Contains(key);
        }

        public System.Collections.Generic.IEnumerator<HashTableNodePair<TKey, TValue>> GetEnumerator()
        {
            foreach (HashTableNodePair<TKey, TValue> value in _array.Items)
                yield return value;
        }

        public int Count
        {
            get
            {
                return _count;
            }
        }
    }
}
