using System.ComponentModel.DataAnnotations;
using Quader.WebApi.Models.Users;

namespace Quader.WebApi.Models.Auth;

public class AuthResponse
{
    public int Id { get; set; }
    public string Username { get; set; }
    public string Email { get; set; }
    public string Token { get; set; }
    public string Role { get; set; }
    public DateTime RegistrationDate { get; set; }

    public AuthResponse(User user, string token)
    {
        Id = user.Id;
        Username = user.Username;
        Email = user.Email;
        Token = token;
        Role = user.Role.RoleName;
        RegistrationDate = user.RegistrationDate;
    }
}