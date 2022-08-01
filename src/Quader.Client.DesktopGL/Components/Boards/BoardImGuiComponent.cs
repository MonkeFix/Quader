using System.Linq;
using ImGuiNET;
using Nez;
using Nez.ImGuiTools;
using Quader.Components.Boards.Renderers;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components.Boards
{
    public class BoardImGuiComponent : Component, IBoardComponent
    {
        public Board Board { get; }

        private bool _isBoardDrawerEnabled = false;
        private BoardDrawerComponent? _boardDrawer;

        private string _stateFileName = "state.json";
        private int _garbageLines = 4;

        public BoardImGuiComponent(Board board)
        {
            Board = board;
        }

        public override void OnAddedToEntity()
        {
            Core.GetGlobalManager<ImGuiManager>()?.RegisterDrawCommand(ImGuiDraw);
        }

        private void ImGuiDraw()
        {
            ImGui.Begin("Piece Handling");
            
            ImGui.Text($"Delta Time ms: {Time.DeltaTime * 1000:F2}\n" +
                       $"Total Time: {Time.TotalTime:F3} seconds\n" +
                       $"Alt Delta Time ms: {Time.AltDeltaTime * 1000:F2}\n" +
                       $"Frame Count: {Time.FrameCount}");
            
            if (ImGui.CollapsingHeader("Time Data"))
            {
                ImGui.Text($"Time Data:");

                var vals = GlobalTimeManager.TimeData.Values;
                foreach (var val in vals)
                {
                    ImGui.Text(val.ToString());
                }

                var totalLast = vals.Select(data => data.LastTime);
                var totalMean = vals.Select(data => data._average);

                double tl = 0, tm = 0;

                foreach (var l in totalLast)
                {
                    tl += l.TotalMilliseconds;
                }

                foreach (var timeSpan in totalMean)
                {
                    tm += timeSpan;
                }

                ImGui.Text($"Total: Last: {tl:F6}, Mean: {tm:F6}");
            }

            ImGui.Separator();

            ImGui.Text($"Board Size: {Board.Width}x{Board.Height}");
            ImGui.Text($"Board Data:\n" +
                       $"Total Height: {Board.TotalHeight}\n" +
                       $"Extra Height: {Board.ExtraHeight}\n" +
                       $"Last Move: {Board.LastMove}, Type: {Board.LastMoveType}" +
                       $"");


            if (ImGui.Button("RESET BOARD"))
                Board.Reset();

            //ImGui.InputInt("Garbage Lines", ref _garbageLines, 1);
            ImGui.DragInt("Garbage Lines", ref _garbageLines, 1, 1, 20);
            if (ImGui.Button("Push Garbage"))
                Board.PushGarbage(_garbageLines);
            ImGui.SameLine();
            if (ImGui.Button("Move Up"))
                Board.MoveUp();
            ImGui.SameLine();
            if (ImGui.Button("Move Down"))
                Board.MoveDown();

            if (ImGui.Button("Send Attack"))
            {
                Board.Attack(_garbageLines);
            }

            ImGui.Separator();

            if (Core.DebugRenderEnabled)
            {
                ImGui.Checkbox("Draw Piece Origin", ref SharedSettings.DrawPieceOrigin);
                ImGui.Checkbox("Draw Bounding Box", ref SharedSettings.DrawPieceBoundingBox);
                ImGui.Checkbox("Draw Piece Test Queue", ref SharedSettings.DrawPieceRotationTests);
            }
            ImGui.Separator();

            var c = Board.CurrentPiece;
            {
                ImGui.Text($"Current Piece: {c.Type}, Position: ({c.X},{c.Y})");
                ImGui.Text($"Bounds: {c.Bounds}");
                ImGui.Text($"Current Rotation: {c.CurrentRotation}");
                //ImGui.Text($"Current Rotation:");
            }

            ImGui.Separator();

            ImGui.Text($"Test Queue Size: {Board.TestQueue.Count}");
            if (ImGui.Button("Queue Next"))
            {
                if (Board.TestQueue.Count > 0)
                {
                    var p = Board.TestQueue.Dequeue();
                    SharedSettings.CurrentTest = p;
                }
                else
                {
                    SharedSettings.CurrentTest = null;
                }
            }

            ImGui.Separator();

            if (!_isBoardDrawerEnabled)
            {
                if (ImGui.Button("ADD BOARD DRAWER"))
                {
                    _boardDrawer = Entity.AddComponent(new BoardDrawerComponent(Board));
                    _isBoardDrawerEnabled = true;
                }
            }
            else
            {
                if (ImGui.Button("REMOVE BOARD DRAWER"))
                {
                    Entity.RemoveComponent<BoardDrawerComponent>();
                    _boardDrawer = null;
                    _isBoardDrawerEnabled = false;
                }

                if (ImGui.Button("Draw 4 Wide"))
                    _boardDrawer?.Draw4Wide();
                if (ImGui.Button("Draw DT Canon"))
                    _boardDrawer?.DrawDtCanon();

                ImGui.InputText("State File Name", ref _stateFileName, 64);

                if (ImGui.Button("Save State"))
                    _boardDrawer?.Save(_stateFileName);
                ImGui.SameLine();
                if (ImGui.Button("Load State"))
                    _boardDrawer?.Load(_stateFileName);
            }


            ImGui.Separator();

            if (ImGui.CollapsingHeader("Piece Spawner"))
            {
                if (ImGui.Button("Spawn I"))
                    Board.SetPiece(PieceType.I);
                ImGui.SameLine();
                if (ImGui.Button("Spawn J"))
                    Board.SetPiece(PieceType.J);
                if (ImGui.Button("Spawn L"))
                    Board.SetPiece(PieceType.L);
                ImGui.SameLine();
                if (ImGui.Button("Spawn T"))
                    Board.SetPiece(PieceType.T);
                if (ImGui.Button("Spawn S"))
                    Board.SetPiece(PieceType.S);
                ImGui.SameLine();
                if (ImGui.Button("Spawn Z"))
                    Board.SetPiece(PieceType.Z);
                if (ImGui.Button("Spawn O"))
                    Board.SetPiece(PieceType.O);
            }

            ImGui.End();
        }
    }
}