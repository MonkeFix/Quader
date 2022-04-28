using System;
using ImGuiNET;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.ImGuiTools;
using Nez.UI;
using Quader.Engine;
using Random = Nez.Random;

namespace Quader.Components
{
    public class BoardComponent : RenderableComponent, IUpdatable
    {
        public override float Width => 1000;
        public override float Height => 1000;


        private Board _board;

        public BoardComponent()
        {
            _board = new Board();
        }

        public override void OnAddedToEntity()
        {
            Core.GetGlobalManager<ImGuiManager>().RegisterDrawCommand(ImGuiDraw);
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var size = 32;

            var c = _board.CurrentPiece;

            for (int y = 0; y < _board.Height; y++)
            {
                for (int x = 0; x < _board.Width; x++)
                {
                    var p = _board.GetPieceAt(x, y);

                    if (p == BoardPieceType.None)
                    {
                        batcher.DrawRect(128 + x * size, 64 + y * size, size, size, Color.Black);
                    }
                    else
                    {
                        batcher.DrawRect(128 + x * size, 64 + y * size, size, size, Color.Red);
                    }

                    batcher.DrawHollowRect(128 + x * size, 64 + y * size, size, size, Color.White * 0.1f, 2f);
                }
            }

            if (c != null)
            {
                for (int y = 0; y < c.PieceTable.GetLength(0); y++)
                {
                    for (int x = 0; x < c.PieceTable.GetLength(1); x++)
                    {
                        if (c.PieceTable[y, x])
                        {
                            batcher.DrawRect(128 + (c.X + x) * size, 64 + (c.Y + y) * size, size, size, c.Color);
                        }
                    }
                }
            }

            /*for (int y = 0; y < _board.Height; y++)
            {
                for (int x = 0; x < _board.Width; x++)
                {
                    var p = _board.GetPieceAt(x, y);

                    if (p == BoardPieceType.None)
                    {
                        batcher.DrawRect(128 + x * size,  64+ y * size, size, size, Color.Black);
                    }
                    else
                    {
                        batcher.DrawRect(128 + x * size, 64 + y * size, size, size, Color.Red);
                    }
                }
            }*/
        }

        public void Update()
        {
            if (Input.IsKeyPressed(Keys.Space))
            {
                _board.HardDrop();
            }

            if (Input.IsKeyDown(Keys.Down))
            {
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
            
            _board.Update(Time.DeltaTime);
        }

        private void ImGuiDraw()
        {
            ImGui.Begin("Spawn Piece");

            if (ImGui.Button("Spawn I"))
                _board.PushPiece(new Piece(PieceType.I));
            if (ImGui.Button("Spawn J"))
                _board.PushPiece(new Piece(PieceType.J));
            if (ImGui.Button("Spawn L"))
                _board.PushPiece(new Piece(PieceType.L));
            if (ImGui.Button("Spawn T"))
                _board.PushPiece(new Piece(PieceType.T));
            if (ImGui.Button("Spawn S"))
                _board.PushPiece(new Piece(PieceType.S));
            if (ImGui.Button("Spawn Z"))
                _board.PushPiece(new Piece(PieceType.Z));
            if (ImGui.Button("Spawn O"))
                _board.PushPiece(new Piece(PieceType.O));

            ImGui.End();
        }
    }
}