using System;
using ImGuiNET;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.ImGuiTools;
using Nez.ImGuiTools.ObjectInspectors;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components
{
    public class BoardComponent : RenderableComponent, IUpdatable
    {
        public override float Width => 800;
        public override float Height => 600;
        
        private Board _board;

        public BoardComponent()
        {
            _board = new Board();
            
            _board.PushPiece(PieceType.T);
        }

        public override void OnAddedToEntity()
        {
            Core.GetGlobalManager<ImGuiManager>().RegisterDrawCommand(ImGuiDraw);
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var size = 32;
            var baseX = 128;
            var baseY = 64;

            var piece = _board.CurrentPiece;
            
            for (int y = 0; y < _board.Height; y++)
            {
                for (int x = 0; x < _board.Width; x++)
                {
                    var p = _board.GetPieceAt(x, y);

                    var drawX = baseX + x * size;
                    var drawY = baseY + y * size;

                    if (p == BoardPieceType.None)
                    {
                        batcher.DrawRect(drawX, drawY, size, size, Color.Black);
                    }
                    else
                    {
                        batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByBoardPieceType(p));
                    }

                    batcher.DrawHollowRect(drawX, drawY, size, size, Color.White * 0.1f, 2f);
                }
            }
            
            if (_board.CurrentPiece != null)
            {
                var curPos = piece.CurrentPos;

                foreach (var p in curPos)
                {
                    var drawX = baseX + (p.X + piece.X) * size;
                    var drawY = baseY + (p.Y + piece.Y) * size;
                    
                    batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(_board.CurrentPiece.Type));
                    //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
                }

                if (_drawOrigin)
                {
                    var t = piece.OffsetType;

                    var pX = baseX + piece.X * size;
                    var pY = baseY + piece.Y * size;

                    if (t == OffsetType.Cell)
                    {
                        pX += 16;
                        pY += 16;
                    }

                    batcher.DrawPixel(pX, pY, Color.White, 10);
                }
                
                if (_drawBoundingBox)
                {
                    var b = piece.Bounds;
                    batcher.DrawHollowRect(baseX + b.X * size, baseY + b.Y * size, b.Width * size, b.Height * size,
                        Color.White, 2f);
                }

                var dropY = _board.FindNearestY();

                var curX = _board.CurrentPiece.X;
                var curY = dropY;
                var points = _board.CurrentPiece.CurrentPos;

                foreach (var p in points)
                {
                    batcher.DrawRect(baseX + (p.X + curX) * size, baseY + (p.Y + curY) * size, size, size,
                        PieceUtils.GetColorByPieceType(_board.CurrentPiece.Type) * 0.5f);
                }
            }
            
            
            if (_currentTest != null && _drawTestQueue)
            {
                foreach (var p in _currentTest)
                {
                    var drawX = baseX + (p.X) * size;
                    var drawY = baseY + (p.Y) * size;
                    
                    batcher.DrawRect(drawX, drawY, size, size, Color.White * 0.5f);
                    //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.Red);
                }
            }
        }

        public void Update()
        {
            if (Input.IsKeyPressed(Keys.Space))
            {
                //var np = boardOld.CreatePiece(boardOld.CurrentPiece.Type);
                //boardOld.HardDrop(np);
                
                _board.HardDrop();
            }

            if (Input.IsKeyDown(Keys.Down))
            {
            }

            if (Input.IsKeyPressed(Keys.Down))
            {
                //_piece.Y += 1;
                
                _board.SoftDrop();
            }

            if (Input.IsKeyPressed(Keys.Left))
            {
                _board.MoveLeft();
            }

            if (Input.IsKeyPressed(Keys.Right))
            {
                _board.MoveRight();
            }

            if (Input.IsKeyPressed(Keys.X))
            {
                _board.Rotate(Rotation.Clockwise);
            }
            if (Input.IsKeyPressed(Keys.Z))
            {
                _board.Rotate(Rotation.CounterClockwise);
            }
            if (Input.IsKeyPressed(Keys.F))
            {
                _board.Rotate(Rotation.Deg180);
            }

            //boardOld.Update(Time.DeltaTime);
        }
        
        private Point[]? _currentTest = null;

        private bool _drawOrigin = true;
        private bool _drawBoundingBox = true;
        private bool _drawTestQueue = true;

        [Inspectable] 
        public int TestProperty { get; set; } = 32;

        [InspectorDelegate]
        public void TestMethod()
        {
            ImGui.TextColored( new System.Numerics.Vector4( 0, 1, 0, 1 ), "Colored text..." );
            ImGui.Combo( "Combo Box", ref privateInt, "First\0Second\0Third\0No Way\0Fifth Option" );
        }

        private int privateInt;
        
        private void ImGuiDraw()
        {
            ImGui.Begin("Piece Handling");
            
            ImGui.Text($"Board Size: {_board.Width}x{_board.Height}");
            if (ImGui.Button("RESET BOARD"))
                _board.Reset();

            ImGui.Separator();

            ImGui.Checkbox("Draw Piece Origin", ref _drawOrigin);
            ImGui.Checkbox("Draw Bounding Box", ref _drawBoundingBox);
            ImGui.Checkbox("Draw Piece Test Queue", ref _drawTestQueue);
            
            ImGui.Separator();

            var c = _board.CurrentPiece;
            if (c != null)
            {
                ImGui.Text($"Current Piece: {c.Type}, Position: ({c.X},{c.Y})");
                ImGui.Text($"Bounds: {c.Bounds}");
                ImGui.Text($"Current Rotation: {c.CurrentRotation}");
                //ImGui.Text($"Current Rotation:");
            }
            
            ImGui.Separator();

            ImGui.Text($"Test Queue Size: {_board.TestQueue.Count}");
            if (ImGui.Button("Queue Next"))
            {
                if (_board.TestQueue.Count > 0)
                {
                    var p = _board.TestQueue.Dequeue();
                    _currentTest = p;
                }
                else
                {
                    _currentTest = null;
                }
            }
            
            ImGui.Separator();

            if (ImGui.Button("Spawn I"))
                //_piece = _pf.Create(PieceType.I);
                _board.PushPiece(PieceType.I);
            ImGui.SameLine();
            if (ImGui.Button("Spawn J"))
                //_piece = _pf.Create(PieceType.J);
                _board.PushPiece(PieceType.J);
            if (ImGui.Button("Spawn L"))
                //_piece = _pf.Create(PieceType.L);
                _board.PushPiece(PieceType.L);
            ImGui.SameLine();
            if (ImGui.Button("Spawn T"))
                //_piece = _pf.Create(PieceType.T);
                _board.PushPiece(PieceType.T);
            if (ImGui.Button("Spawn S"))
                //_piece = _pf.Create(PieceType.S);
                _board.PushPiece(PieceType.S);
            ImGui.SameLine();
            if (ImGui.Button("Spawn Z"))
                //_piece = _pf.Create(PieceType.Z);
                _board.PushPiece(PieceType.Z);
            if (ImGui.Button("Spawn O"))
                //_piece = _pf.Create(PieceType.O);
                _board.PushPiece(PieceType.O);

            ImGui.End();
        }
    }
}