using System;
using System.Collections.Generic;
using System.Linq;

public class GradeSchool
{

    // My Solution commented below: it's good, but not great, great solution by ENDR using LINQ

    private List<RosterEntry> _schoolRoster = new List<RosterEntry>();

    public void Add(string student, int grade) => _schoolRoster.Add(new RosterEntry() { Name = student, Grade = grade });

    public IEnumerable<string> Roster() => _schoolRoster.OrderBy(s => s.Grade).ThenBy(s => s.Name).Select(s => s.Name);

    public IEnumerable<string> Grade(int grade) => _schoolRoster.Where(s => s.Grade == grade).Select(s => s.Name).OrderBy(s => s);

    //private SortedDictionary<int, List<string>> gradeSchoolDict;
    //public GradeSchool()
    //{
    //    gradeSchoolDict = new SortedDictionary<int, List<string>>();
    //}
    //public void Add(string student, int grade)
    //{
    //    var containsKey = gradeSchoolDict.ContainsKey(grade);

    //    if (containsKey)
    //    {
    //        var currentStudents = gradeSchoolDict[grade];
    //        currentStudents.Add(student);
    //        currentStudents.Sort();
    //    }
    //    else
    //    {
    //        gradeSchoolDict.Add(grade, new List<string>() { student });
    //    }
    //}

    //public IEnumerable<string> Roster()
    //{
    //    var result = new List<string>();
    //    foreach(var key in gradeSchoolDict.Keys)
    //    {
    //        gradeSchoolDict[key].ForEach((string student) =>
    //        {
    //            result.Add(student);
    //        });
    //    }

    //    return result;
    //}

    //public IEnumerable<string> Grade(int grade)
    //{
    //    var containsKey = gradeSchoolDict.ContainsKey(grade);
    //    if (containsKey)
    //    {
    //        return gradeSchoolDict[grade];
    //    }
    //    else
    //    {
    //        return new List<string>();
    //    }
    //}
}

internal class RosterEntry
{
    public string Name { get; set; }
    public int Grade { get; set; }
}