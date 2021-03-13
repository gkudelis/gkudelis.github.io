use cement::prototypes::file_select;

fn main() {
    for post_path in file_select::files_matching("posts/*.md") {
        println!("{}", post_path.display());
    }
}
