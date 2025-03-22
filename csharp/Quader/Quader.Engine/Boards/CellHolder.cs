using System.Collections;
using System.Runtime.CompilerServices;
using Quader.Engine.Pieces;
using Quader.Engine.Primitives;

namespace Quader.Engine.Boards;

public class CellHolder
{
    public struct Row : IEnumerable<CellType>
    {
        public List<CellType> Cells { get; }
        public int Width { get; }

        public Row() : this(CellType.None, 10)
        {
        }

        public Row(CellType fillWith, int width)
        {
            Cells = new List<CellType>(width);
            Width = width;
            for (int i = 0; i < 10; i++)
                Cells.Add(fillWith);
        }

        public Row(List<CellType> cells, int width)
        {
            Cells = cells;
            Width = width;
        }

        public CellType Set(int x, CellType cellType)
        {
            var tmp = Cells[x];
            Cells[x] = cellType;

            return tmp;
        }

        public CellType Get(int x) => Cells[x];

        public bool IsFull() => Cells.All(b => b != CellType.None && b != CellType.Solid);
        public bool IsEmpty() => Cells.All(b => b == CellType.None);
        public int GetOccupiedCellCount() => Cells.Where(IncreasesCellCount).Count();

        public void Clear()
        {
            for (int i = 0; i < Cells.Count; i++)
                Cells[i] = CellType.None;
        }

        public static bool IncreasesCellCount(CellType type) =>
            type != CellType.None && type != CellType.Solid;

        public static Row Empty => new Row();
        public static Row Solid => new Row(CellType.Solid, 10);
        public static Row Garbage => new Row(CellType.Garbage, 10);

        public IEnumerator<CellType> GetEnumerator()
        {
            return Cells.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    public int Width { get; }
    public int Height { get; }
    private List<Row> _layout;
    private int _occupiedCells;
    public int OccupiedCells => _occupiedCells;
    public List<Row> Layout => _layout;

    public CellHolder(BoardSettings boardSettings)
    {
        Width = boardSettings.Width;
        Height = boardSettings.FullHeight;
        _layout = new List<Row>();
        _occupiedCells = 0;

        Reset();
    }

    public void Reset()
    {
        _layout.Clear();
        for (int i = 0; i < Height; i++)
        {
            _layout.Add(Row.Empty);
        }

        _occupiedCells = 0;
    }

    public void Clear()
    {
        foreach (var row in _layout)
        {
            row.Clear();
        }

        _occupiedCells = 0;
    }

    public List<int> CheckRowClears(Rectangle? bounds)
    {
        int max;
        if (bounds.HasValue) max = bounds.Value.Top;
        else max = 0;

        var result = new List<int>();

        for (int i = Math.Max(0, max); i < Height; i++)
        {
            if (_layout[i].IsFull())
                result.Add(i);
        }

        return result;
    }

    public bool IsRowFull(int y) => _layout[y].IsFull();
    public CellType GetCellAt(int x, int y) => _layout[y].Get(x);

    public void SetCellAt(int x, int y, CellType cell)
    {
        var old = _layout[y].Set(x, cell);

        if (Row.IncreasesCellCount(cell) && !Row.IncreasesCellCount(old))
            _occupiedCells += 1;
    }

    public bool IsOutOfBounds(int x, int y) => x < 0 || x >= Width || y >= Height || y < 0;

    public bool Intersects(Point p) =>
        IsOutOfBounds(p.X, p.Y) || GetCellAt(p.X, p.Y) != CellType.None;

    public bool IntersectsAny(IEnumerable<Point> points)
    {
        foreach (var point in points)
        {
            if (Intersects(point))
                return true;
        }

        return false;
    }

    public void MoveUp(bool updateCellCount = true)
    {
        for (int i = 1; i < Height; i++)
        {
            var cur = _layout[i];

            _layout[i] = Row.Empty;
            _layout[i - 1] = cur;
        }

        if (updateCellCount)
            _occupiedCells += Width;
    }

    public void MoveDown(int fromY, bool updateCellCount = true)
    {
        for (int i = fromY - 1; i >= 0; i--)
        {
            var cur = _layout[i];

            _layout[i] = Row.Empty;
            _layout[i + 1] = cur;
        }

        if (updateCellCount)
            _occupiedCells -= Width;
    }

    public Row CreateGarbageRow(int holeX)
    {
        var res = Row.Garbage;
        res.Set(holeX, CellType.None);
        return res;
    }

    public void PushGarbage(int holeX)
    {
        MoveUp(false);
        SetRow(Height - 1, CreateGarbageRow(holeX));

        _occupiedCells += Width - 1;
    }

    public void ClearRows(IEnumerable<int> ys)
    {
        foreach (var y in ys)
            MoveDown(y);
    }

    public Row GetRow(int y) => _layout[y];

    public void SetRow(int y, Row row) => _layout[y] = row;

    public int CalcNearestY(int curX, int curY, Point[] points)
    {
        var y = curY;

        for (int i = curY; i <= Height; i++)
        {
            var offset = new Point(curX, curY);
            var newPoints = PieceHelpers.AdjustPositions(points, offset);
            if (IntersectsAny(newPoints))
                break;
            y = i;
        }

        return y;
    }
}