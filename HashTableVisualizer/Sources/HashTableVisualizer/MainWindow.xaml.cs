using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.IO;
using System.Xml.Linq;
using System.Text.RegularExpressions;

namespace HashTableVisualizer
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        HashTable<string, ListViewData> Hash = new HashTable<string, ListViewData>();
        bool dataChanged = false;

        public void Save(System.Windows.Data.CollectionView items)
        {
            XDocument xdoc = new XDocument();
            XElement xeRoot = new XElement("Data");

            foreach (var item in items)
            {
                ListViewData lvc = (ListViewData)item;

                XElement xRow = new XElement("Row");
                xRow.Add(new XElement("Id", lvc.Id));
                xRow.Add(new XElement("Name", lvc.Name));
                xRow.Add(new XElement("Count", lvc.Count));

                xeRoot.Add(xRow);
            }
            xdoc.Add(xeRoot);

            xdoc.Save("ListViewData.xml");
        }

        public IEnumerable<ListViewData> GetRows()
        {
            List<ListViewData> rows = new List<ListViewData>();

            if (File.Exists("ListViewData.xml"))
            {
                var rowsFromFile = from c in XDocument
                                   .Load("ListViewData.xml")
                                   .Elements("Data")
                                   .Elements("Row")
                                   select c;

                foreach (var row in rowsFromFile)
                {
                    rows.Add(new ListViewData(
                        id: row.Element("Id").Value,
                        name: row.Element("Name").Value,
                        count: row.Element("Count").Value));
                }
            }

            return rows;
        }

        private void ShowData()
        {
            profitList.Items.Clear();

            foreach (var row in Hash)
                profitList.Items.Add(row.Value);
        }

        private void addButton_Click(object sender, RoutedEventArgs e)
        {
            ListViewData val = new ListViewData(
                id: idText.Text,
                name: nameText.Text,
                count: countText.Text);

            try
            {
                Hash.Add(val.Id, val);
                idText.Text = "";
                dataChanged = true;
                ShowData();
            }
            catch (ArgumentException exc)
            {
                MessageBox.Show(
                    exc.Message,
                    this.Title,
                    MessageBoxButton.OK,
                    MessageBoxImage.Error);
            }
        }

        private void removeButton_Click(object sender, RoutedEventArgs e)
        {
            if (!Hash.Remove(idText.Text)) return;

            idText.Text = "";
            dataChanged = true;
            ShowData();

            MessageBox.Show(
                "Значение было успешно удалено!",
                this.Title,
                MessageBoxButton.OK,
                MessageBoxImage.Information);
        }

        private void searchButton_Click(object sender, RoutedEventArgs e)
        {
            nameText.Text = countText.Text = "";

            ListViewData val = Hash.ContainsKey(idText.Text);

            if (val == null) return;

            nameText.Text = val.Name;
            countText.Text = val.Count;

            MessageBox.Show(
                "Значение было успешно найдено!",
                this.Title,
                MessageBoxButton.OK,
                MessageBoxImage.Information);
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            foreach (var row in GetRows())
                Hash.Add(row.Id, row);

            ShowData();
        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (!dataChanged) return;

            string message = "Хотите ли сохранить изменения?";
            MessageBoxResult result = MessageBox.Show(message,
                    this.Title,
                    MessageBoxButton.YesNoCancel, MessageBoxImage.Question);

            if (result == MessageBoxResult.Yes)
                Save(profitList.Items);
            else if (result == MessageBoxResult.Cancel)
                e.Cancel = true;
        }

        private static bool IsTextAllowed(string text)
        {
            // Можно пользовать в PreviewTextInput
            // if (!char.IsDigit(e.Text, e.Text.Length - 1))

            Regex regex = new Regex("[^0-9]+");
            return !regex.IsMatch(text);
        }

        private void PreviewTextInputHandler(object sender, TextCompositionEventArgs e)
        {
            e.Handled = !IsTextAllowed(e.Text);
        }

        private void PastingHandler(object sender, DataObjectPastingEventArgs e)
        {
            if (e.DataObject.GetDataPresent(typeof(String)))
            {
                String text = (String)e.DataObject.GetData(typeof(String));
                if (!IsTextAllowed(text))
                    e.CancelCommand();
            }
            else
                e.CancelCommand();
        }

        private void PreviewKeyDownHandler(object sender, KeyEventArgs e)
        {
            e.Handled = e.Key == System.Windows.Input.Key.Space;
        }

        private void profitList_PreviewMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            var selected = profitList.SelectedItem as ListViewData;

            if (selected != null)
                idText.Text = selected?.Id;
        }

        private void TextChangedHandler(object sender, TextChangedEventArgs e)
        {
            if (idText.Text == "")
            {
                addButton.IsEnabled = false;
                removeButton.IsEnabled = false;
                searchButton.IsEnabled = false;
            }
            else
            {
                addButton.IsEnabled = nameText.Text.Trim() != "" && countText.Text != "";

                if (Hash.Count > 0)
                {
                    removeButton.IsEnabled = true;
                    searchButton.IsEnabled = true;
                }
            }
        }

        private void aboutButton_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show(
                "Автор: Константинов О. В.\n" +
                "Преподаватель: Елсукова Е. А.\n" +
                "Группа: Б8219\n" +
                "Год: 2017",
                "О программе...");
        }
    }
}
