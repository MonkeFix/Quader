using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Microsoft.AspNetCore.Identity;
using Newtonsoft.Json;

namespace Quader.WebApi.Models.Users;

public class User
{
    [Key]
    public int Id { get; init; }

    [Required] [ProtectedPersonalData] public string Username { get; init; } = null!;
    [Required] [ProtectedPersonalData] [EmailAddress]
    public string Email { get; set; } = null!;

    [Required]
    [DataType(DataType.DateTime)]
    [DisplayFormat(DataFormatString = "{0:yyyy-MM-dd}", ApplyFormatInEditMode = true)]
    public DateTime RegistrationDate { get; set; }


    [Required]
    [DataType(DataType.Password)]
    [JsonIgnore]
    public string PasswordHash { get; set; } = null!;

    [JsonIgnore] [NotMapped] public string Password { get; set; } = null!;

    public int RoleId { get; set; }
    [Required] public UserRole Role { get; set; } = null!;
}