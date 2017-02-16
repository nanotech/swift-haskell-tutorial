#import "SwiftAppLibrary.h"

@interface AClassInThisFramework : NSObject @end
@implementation AClassInThisFramework @end

void runNSApplication(void) {
    NSApplication *app = [NSApplication sharedApplication];
    NSBundle *bundle = [NSBundle bundleForClass:[AClassInThisFramework class]];
    NSArray *topObjects;
    [[[NSNib alloc] initWithNibNamed:@"MainMenu" bundle:bundle]
     instantiateWithOwner:app topLevelObjects:&topObjects];
    [app run];
}
