using System;
using System.IO;
using Microsoft.Xna.Framework;
using Nez;
using Nez.Persistence;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Serialization;
using Quader.Skinning;

namespace Quader.Components.Boards.Renderers
{
    public class BoardDrawerComponent : Component, IUpdatable, IBoardComponent
    {
        public Board Board { get; }

        private BoardSkin _boardSkin;

        public BoardDrawerComponent(Board board)
        {
            Board = board;
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        }

        public void Draw4Wide()
        {
            
        }

        public void DrawDtCanon()
        {

        }

        public void Save(string filename)
        {
            using var sw = new StreamWriter(filename, false);

            var encoded = Board.Encode();
            var json = Json.ToJson(encoded, true);
            sw.WriteLine(json);
        }

        public void Load(string filename)
        {
            if (string.IsNullOrEmpty(filename))
                throw new Exception("No filename has been provided");

            if (!File.Exists(filename))
                throw new FileNotFoundException();

            using var sr = new StreamReader(filename);
            var content = Json.FromJson<BoardEncoding>(sr.ReadToEnd());

            Board.Decode(content);
        }

        public void Update()
        {
            var mp = Input.MousePosition;

            var scaledMp = new Point(
                (int)((mp.X - Entity.Position.X * Entity.Scale.X) / _boardSkin.CellSize),
                (int)(((mp.Y - Entity.Position.Y + Board.ExtraHeight * _boardSkin.CellSize) * Entity.Scale.Y) /
                      _boardSkin.CellSize)
            );

            if (Input.LeftMouseButtonDown && !Board.IsOutOfBounds(scaledMp))
            {
                Board.SetCellAt(scaledMp.X, scaledMp.Y, BoardCellType.Garbage, true);
            }
            else if (Input.RightMouseButtonDown && !Board.IsOutOfBounds(scaledMp))
            {
                Board.SetCellAt(scaledMp.X, scaledMp.Y, BoardCellType.None, true);
            }
        }
    }
}