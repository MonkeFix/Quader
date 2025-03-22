using System.Diagnostics.CodeAnalysis;

// ReSharper disable once CheckNamespace
namespace Nez
{
    /// <summary>
    /// Class that contains the names of all of the files processed by the Pipeline Tool
    /// </summary>
    /// <remarks>
    /// Nez includes a T4 template that will auto-generate the content of this file.
    /// See: https://github.com/prime31/Nez/blob/master/FAQs/ContentManagement.md#auto-generating-content-paths"
    /// </remarks>
    [SuppressMessage("ReSharper", "UnusedMember.Global")]
    [SuppressMessage("ReSharper", "InconsistentNaming")]
    [SuppressMessage("ReSharper", "IdentifierTypo")]
    class Content
    {
        public static class Fonts
        {
            public const string Debug_font = @"Content/Fonts/debug_font.fnt";
            public const string Debug_font_0 = @"Content/Fonts/debug_font_0.png";
            public const string Font_cfg = @"Content/Fonts/font_cfg.bmfc";
            public const string Main_font = @"Content/Fonts/main_font.fnt";
            public const string Main_font_0 = @"Content/Fonts/main_font_0.png";
            public const string Silkscreen8 = @"Content/Fonts/silkscreen8.fnt";
            public const string Silkscreen8_0 = @"Content/Fonts/silkscreen8_0.png";
        }

        public static class Skins
        {
            public const string Board_default = @"Content/Skins/board_default.png";
            public const string Damage_cell = @"Content/Skins/damage_cell.png";

            public const string Damage_meter_background =
                @"Content/Skins/damage_meter_background.png";

            public const string Default = @"Content/Skins/default.png";
            public const string Default_2 = @"Content/Skins/default_2.png";
            public const string Default_3 = @"Content/Skins/default_3.png";
        }
    }
}