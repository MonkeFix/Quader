using System;
using ImGuiNET;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.ImGuiTools;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Quader.Engine.RotationEncoder;
using Random = Nez.Random;

namespace Quader.Components
{
    public class BoardComponent : RenderableComponent, IUpdatable
    {
        public override float Width => 800;
        public override float Height => 600;


        private BoardOld boardOld;

        private Board _board;

        public BoardComponent(RotationSystemTable rts)
        {
            boardOld = new BoardOld(rts);
            _board = new Board();
            
            _board.PushPiece(PieceType.T);
        }

        public override void OnAddedToEntity()
        {
            Core.GetGlobalManager<ImGuiManager>().RegisterDrawCommand(ImGuiDraw);
        }

        private PieceBase _piece = new PieceZ();

        public override void Render(Batcher batcher, Camera camera)
        {
            /*var size = 32;

            var c = boardOld.CurrentPiece;

            for (int y = 0; y < boardOld.Height; y++)
            {
                for (int x = 0; x < boardOld.Width; x++)
                {
                    var p = boardOld.GetPieceAt(x, y);

                    if (p == BoardPieceType.None)
                    {
                        batcher.DrawRect(128 + x * size, 64 + y * size, size, size, Color.Black);
                    }
                    else
                    {
                        batcher.DrawRect(128 + x * size, 64 + y * size, size, size, PieceUtils.GetColorByBoardPieceType(p));
                    }

                    batcher.DrawHollowRect(128 + x * size, 64 + y * size, size, size, Color.White * 0.1f, 2f);
                }
            }

            if (c != null)
            {
                for (int y = 0; y < c.PieceTable.Length; y++)
                {
                    for (int x = 0; x < c.PieceTable.Length; x++)
                    {
                        if (c.DrawAt(x, y))
                        {
                            batcher.DrawRect(128 + (c.X + x) * size, 64 + (c.Y + y) * size, size, size, c.Color);
                        }
                    }
                }
            }

            // Draw Ghost Piece
            if (c != null)
            {
                var nY = boardOld.FindNearestDropY(); // Unoptimized as fuck, check only on piece movement

                for (int y = 0; y < c.PieceTable.Length; y++)
                {
                    for (int x = 0; x < c.PieceTable.Length; x++)
                    {
                        if (c.DrawAt(x, y))
                        {
                            batcher.DrawRect(128 + (c.X + x) * size, 64 + (nY + y) * size, size, size, c.Color * 0.5f);
                        }
                    }
                }
            }*/
            
            /*var w = 32;
            var h = 32;
            var baseX = 128 + _piece.X * w;
            var baseY = 64 + _piece.Y * h;

            var curPos = _piece.CurrentPos;

            for (int i = 0; i < curPos.Length; i++)
            {
                var point = curPos[i];
                
                batcher.DrawRect(baseX + point.X * w, baseY + point.Y * h, w, h, _piece.BaseColor);
                batcher.DrawHollowRect(baseX + point.X * w, baseY + point.Y * h, w, h, Color.Black * 0.5f, 4f);
            }

            var t = _piece.OffsetType;

            if (t == OffsetType.Cell)
            {
                baseX += w / 2;
                baseY += h / 2;
            }

            batcher.DrawPixel(baseX, baseY, Color.White, 12);

            var b = _piece.Bounds;
            batcher.DrawHollowRect( 128 + b.X * w, 64 + b.Y * h, b.Width * w, b.Height * h, Color.White, 2f);*/

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
            
            if (piece != null)
            {
                var curPos = piece.CurrentPos;

                foreach (var p in curPos)
                {
                    var drawX = baseX + (p.X + piece.X) * size;
                    var drawY = baseY + (p.Y + piece.Y) * size;
                    
                    batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(_board.CurrentPiece.Type));
                    //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
                }
                
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
        }

        public void Update()
        {
            if (Input.IsKeyPressed(Keys.Space))
            {
                var np = boardOld.CreatePiece(boardOld.CurrentPiece.Type);
                boardOld.HardDrop(np);
            }

            if (Input.IsKeyDown(Keys.Down))
            {
                boardOld.SoftDrop();
            }

            if (Input.IsKeyPressed(Keys.Down))
            {
                //_piece.Y += 1;
                
                _board.SoftDrop();
            }

            if (Input.IsKeyPressed(Keys.Left))
            {
                boardOld.MoveLeft();
                // _piece.X -= 1;
                _board.MoveLeft();
            }

            if (Input.IsKeyPressed(Keys.Right))
            {
                boardOld.MoveRight();
                //_piece.X += 1;
                _board.MoveRight();
            }

            if (Input.IsKeyPressed(Keys.X))
            {
                boardOld.RotatePiece(Rotation.Clockwise);
                //_piece.RotateSimple(Rotation.Clockwise);

                _board.Rotate(Rotation.Clockwise);
            }
            if (Input.IsKeyPressed(Keys.Z))
            {
                boardOld.RotatePiece(Rotation.CounterClockwise);
                //_piece.RotateSimple(Rotation.CounterClockwise);
                
                _board.Rotate(Rotation.CounterClockwise);
            }
            if (Input.IsKeyPressed(Keys.F))
            {
                boardOld.RotatePiece(Rotation.Deg180);
                // _piece.RotateSimple(Rotation.Deg180);

                _board.Rotate(Rotation.Deg180);
            }

            boardOld.Update(Time.DeltaTime);
        }

        private PieceFactory _pf = new ();
        
        private void ImGuiDraw()
        {
            ImGui.Begin("Spawn Piece");
            
            ImGui.Text($"Board Size: {_board.Width}x{_board.Height}");

            var c = _board.CurrentPiece;
            if (c != null)
            {
                ImGui.Text($"Current Piece: {c.Type}, Position: ({c.X},{c.Y})");
                ImGui.Text($"Bounds: {c.Bounds}");
                ImGui.Text($"Current Rotation: {c.CurrentRotation}");
                //ImGui.Text($"Current Rotation:");
            }

            if (ImGui.Button("Spawn I"))
                //_piece = _pf.Create(PieceType.I);
                _board.PushPiece(PieceType.I);
            if (ImGui.Button("Spawn J"))
                //_piece = _pf.Create(PieceType.J);
                _board.PushPiece(PieceType.J);
            if (ImGui.Button("Spawn L"))
                //_piece = _pf.Create(PieceType.L);
                _board.PushPiece(PieceType.L);
            if (ImGui.Button("Spawn T"))
                //_piece = _pf.Create(PieceType.T);
                _board.PushPiece(PieceType.T);
            if (ImGui.Button("Spawn S"))
                //_piece = _pf.Create(PieceType.S);
                _board.PushPiece(PieceType.S);
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