using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Quader.Engine.RotationEncoder;

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
        
        public Color BaseColor => PieceUtils.GetColorByPieceType(Type);

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

        public virtual Dictionary<PieceRotationType, Point[]> WallKickData { get; } = PieceUtils.DefaultWallKickData;

        public class WallKickCheckParams
        {
            public Point[] Tests { get; set; }
            public Point[] ExpectedPos { get; set; }
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

        private void RotateSimple(Rotation rotation)
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

        private Point[] GetWallKickByRotation(Rotation rotation, out Point[] expectedPos)
        {
            var res = GetRotationTypeByRotation(rotation);
            expectedPos = res.pos;
            
            return WallKickData[res.rotationType];
        }

        private (PieceRotationType rotationType, Point[] pos) GetRotationTypeByRotation(Rotation rotation)
        {
            switch (CurrentRotation)
            {
                case PieceStartPosition.Initial:
                    switch (rotation)
                    {
                        case Rotation.Clockwise:
                            return (PieceRotationType.SpawnToRight, RightPos);
                        case Rotation.CounterClockwise:
                            return (PieceRotationType.SpawnToLeft, LeftPos);
                        case Rotation.Deg180:
                            return (PieceRotationType.RightToDeg180, Deg180Pos);
                        default:
                            throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
                    }
                case PieceStartPosition.RotationClockwise:
                    switch (rotation)
                    {
                        case Rotation.Clockwise:
                            return (PieceRotationType.RightToDeg180, Deg180Pos);
                        case Rotation.CounterClockwise:
                            return (PieceRotationType.RightToSpawn, SpawnPos);
                        case Rotation.Deg180:
                            return (PieceRotationType.Deg180ToLeft, LeftPos);
                        default:
                            throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
                    }
                case PieceStartPosition.Rotation180Deg:
                    switch (rotation)
                    {
                        case Rotation.Clockwise:
                            return (PieceRotationType.Deg180ToLeft, LeftPos);
                        case Rotation.CounterClockwise:
                            return (PieceRotationType.Deg180ToRight, RightPos);
                        case Rotation.Deg180:
                            return (PieceRotationType.LeftToSpawn, SpawnPos);
                        default:
                            throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
                    }
                case PieceStartPosition.RotationCounterClockwise:
                    switch (rotation)
                    {
                        case Rotation.Clockwise:
                            return (PieceRotationType.LeftToSpawn, SpawnPos);
                        case Rotation.CounterClockwise:
                            return (PieceRotationType.LeftToDeg180, Deg180Pos);
                        case Rotation.Deg180:
                            return (PieceRotationType.SpawnToRight, RightPos);
                        default:
                            throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
                    }
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        private void RotateRight()
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

        private void RotateLeft()
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