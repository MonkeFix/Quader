using Quader.WebApi.Models.Users;

namespace Quader.WebApi.Services.UserManagement;

public interface IUserService
{
    Task<IEnumerable<User>> GetAllAsync();
    Task<User> GetByIdAsync(int id);
    Task<User> GetByUsernameAsync(string username);
    Task<User> GetByEmailAsync(string email);
    Task<User> RegisterAsync(UserRegisterRequest model);
    Task ChangePasswordByIdAsync(int id, string oldPassword, string newPassword);
    Task<User> UpdateAsync(User user);
    Task UpdateRoleByUsernameAsync(string username, string newRoleName);
}