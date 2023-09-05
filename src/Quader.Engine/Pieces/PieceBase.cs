using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;

namespace Quader.Engine.Pieces
{
    public enum OffsetType
    {
        Cell,
        BetweenCells
    }
    
    public abstract class PieceBase
    {
        public abstract PieceType Type { get; }
        public abstract BoardCellType BoardCellType { get; }
        /// <summary>
        /// Gets occupied piece cells for the <b>Spawn</b> position, where (0,0) is the origin
        /// </summary>
        protected abstract Point[] SpawnPos { get; }
        /// <summary>
        /// Gets occupied piece cells for the <b>Right (Clockwise)</b> position, where (0,0) is the origin
        /// </summary>
        protected abstract Point[] RightPos { get; }
        /// <summary>
        /// Gets occupied piece cells for the <b>180 Degrees (double right)</b> position, where (0,0) is the origin
        /// </summary>
        protected abstract Point[] Deg180Pos { get; }
        /// <summary>
        /// Gets occupied piece cells for the <b>Left (Counter-Clockwise)</b> position, where (0,0) is the origin
        /// </summary>
        protected abstract Point[] LeftPos { get; }

        public virtual OffsetType OffsetType { get; } = OffsetType.Cell;

        public Rectangle Bounds
        {
            get
            {
                if (_bounds == null)
                    _bounds = GetBounds();

                return _bounds.Value;
            }
        } 

        private Rectangle? _bounds = null;
        
        public virtual Color BaseColor => PieceUtils.GetColorByPieceType(Type);

        public Point[] CurrentPos
        {
            get
            {
                return CurrentRotation switch
                {
                    PieceStartPosition.Initial => SpawnPos,
                    PieceStartPosition.RotationClockwise => RightPos,
                    PieceStartPosition.Rotation180Deg => Deg180Pos,
                    PieceStartPosition.RotationCounterClockwise => LeftPos,
                    _ => throw new ArgumentOutOfRangeException()
                };
            }
        }

        private int _x;
        public int X
        {
            get => _x;
            set
            {
                _x = value;
                _bounds = GetBounds();
            }
        }
        private int _y;
        public int Y
        {
            get => _y;
            set
            {
                _y = value;
                _bounds = GetBounds();
            }
        }

        public PieceStartPosition CurrentRotation { get; set; } = PieceStartPosition.Initial;

        public virtual Dictionary<PieceRotationType, Point[]> WallKickData { get; } = PieceUtils.PieceSettings!.DefaultWallKickData;

        public class WallKickCheckParams
        {
            public Point[] Tests { get; set; } = null!;
            public Point[] ExpectedPos { get; set; } = null!;
        }
        
        public class WallKickCheckResult
        {
            public bool Success { get; set; }
            public Point? WallKickPosition { get; set; }
        }
        
        public void Rotate(Rotation rotation, Func<WallKickCheckParams, WallKickCheckResult> wallKickPredicate)
        {
            var r = GetRotationTypeByRotation(rotation);
            var tests = WallKickData[r.rotationType];
            
            var wkp = new WallKickCheckParams
            {
                Tests = tests,
                ExpectedPos = r.pos
            };
            
            var result = wallKickPredicate(wkp);

            if (!result.Success || result.WallKickPosition == null)
                return;
            
            RotateSimple(rotation);
            
            
            X += result.WallKickPosition.Value.X;
            Y += result.WallKickPosition.Value.Y;

            _bounds = GetBounds();
        }

        protected void RotateSimple(Rotation rotation)
        {
            switch (rotation)
            {
                case Rotation.Clockwise:
                    RotateRight();
                    break;
                case Rotation.CounterClockwise:
                    RotateLeft();
                    break;
                case Rotation.Deg180:
                    RotateRight();
                    RotateRight();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
            }
            
            _bounds = GetBounds();
        }

        private Rectangle GetBounds()
        {
            return GetBounds(CurrentPos, X, Y);
        }

        public static Rectangle GetBounds(Point[] positions, int x, int y)
        {
            var t = positions;
            
            var minX = t.Min(p => p.X);
            var minY = t.Min(p => p.Y);
            var maxX = t.Max(p => p.X);
            var maxY = t.Max(p => p.Y);
            
            var w = 1 + (minX == maxX ? 0 : Math.Abs(minX) + Math.Abs(maxX));
            var h = 1 + (minY == maxY ? 0 : Math.Abs(minY) + Math.Abs(maxY));
            
            return new Rectangle(x + minX, y + minY, w, h);
        }

        protected Point[] GetWallKickByRotation(Rotation rotation, out Point[] expectedPos)
        {
            var res = GetRotationTypeByRotation(rotation);
            expectedPos = res.pos;
            
            return WallKickData[res.rotationType];
        }

        protected (PieceRotationType rotationType, Point[] pos) GetRotationTypeByRotation(Rotation rotation)
        {
            return CurrentRotation switch
            {
                PieceStartPosition.Initial => rotation switch
                {
                    Rotation.Clockwise => (PieceRotationType.SpawnToRight, RightPos),
                    Rotation.CounterClockwise => (PieceRotationType.SpawnToLeft, LeftPos),
                    Rotation.Deg180 => (PieceRotationType.SpawnToDeg180, Deg180Pos),
                    _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
                },
                PieceStartPosition.RotationClockwise => rotation switch
                {
                    Rotation.Clockwise => (PieceRotationType.RightToDeg180, Deg180Pos),
                    Rotation.CounterClockwise => (PieceRotationType.RightToSpawn, SpawnPos),
                    Rotation.Deg180 => (PieceRotationType.Deg180ToLeft, LeftPos),
                    _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
                },
                PieceStartPosition.Rotation180Deg => rotation switch
                {
                    Rotation.Clockwise => (PieceRotationType.Deg180ToLeft, LeftPos),
                    Rotation.CounterClockwise => (PieceRotationType.Deg180ToRight, RightPos),
                    Rotation.Deg180 => (PieceRotationType.Deg180ToSpawn, SpawnPos),
                    _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
                },
                PieceStartPosition.RotationCounterClockwise => rotation switch
                {
                    Rotation.Clockwise => (PieceRotationType.LeftToSpawn, SpawnPos),
                    Rotation.CounterClockwise => (PieceRotationType.LeftToDeg180, Deg180Pos),
                    Rotation.Deg180 => (PieceRotationType.SpawnToRight, RightPos),
                    _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
                },
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        protected void RotateRight()
        {
            CurrentRotation = CurrentRotation switch
            {
                PieceStartPosition.Initial => PieceStartPosition.RotationClockwise,
                PieceStartPosition.RotationClockwise => PieceStartPosition.Rotation180Deg,
                PieceStartPosition.Rotation180Deg => PieceStartPosition.RotationCounterClockwise,
                PieceStartPosition.RotationCounterClockwise => PieceStartPosition.Initial,
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        protected void RotateLeft()
        {
            CurrentRotation = CurrentRotation switch
            {
                PieceStartPosition.Initial => PieceStartPosition.RotationCounterClockwise,
                PieceStartPosition.RotationClockwise => PieceStartPosition.Initial,
                PieceStartPosition.RotationCounterClockwise => PieceStartPosition.Rotation180Deg,
                PieceStartPosition.Rotation180Deg => PieceStartPosition.RotationClockwise,
                _ => throw new ArgumentOutOfRangeException()
            };
        }
    }
}