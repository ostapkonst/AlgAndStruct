using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HashTableVisualizer
{
    public class HashTableArrayNode<TKey, TValue>
    {
        LinkedList<HashTableNodePair<TKey, TValue>> _items;

        public void Add(TKey key, TValue value)
        {
            if (_items == null)
                _items = new LinkedList<HashTableNodePair<TKey, TValue>>();
            else
                foreach (HashTableNodePair<TKey, TValue> pair in _items)
                    if (pair.Key.Equals(key))
                        throw new ArgumentException("Такой ключ уже существует!");

            _items.AddLast(new HashTableNodePair<TKey, TValue>(key, value));
        }

        public void Update(TKey key, TValue value)
        {
            bool updated = false;

            if (_items != null)
                foreach (HashTableNodePair<TKey, TValue> pair in _items)
                    if (pair.Key.Equals(key))
                    {
                        pair.Value = value;
                        updated = true;
                        break;
                    }

            if (!updated)
                throw new ArgumentException("Такой ключ не был найден!");
        }

        public bool TryGetValue(TKey key, out TValue value)
        {
            value = default(TValue);
            bool found = false;

            if (_items != null)
                foreach (HashTableNodePair<TKey, TValue> pair in _items)
                    if (pair.Key.Equals(key))
                    {
                        value = pair.Value;
                        found = true;
                        break;
                    }

            return found;
        }

        public bool Remove(TKey key)
        {
            bool removed = false;

            if (_items != null)
            {
                LinkedListNode<HashTableNodePair<TKey, TValue>> current = _items.First;

                while (current != null)
                {
                    if (current.Value.Key.Equals(key))
                    {
                        _items.Remove(current);
                        removed = true;
                        break;
                    }
                    current = current.Next;
                }
            }

            return removed;
        }

        public void Clear()
        {
            if (_items != null)
                _items.Clear();
        }

        public IEnumerable<HashTableNodePair<TKey, TValue>> Items
        {
            get
            {
                if (_items != null)
                    foreach (HashTableNodePair<TKey, TValue> node in _items)
                        yield return node;
            }
        }
    }
}
