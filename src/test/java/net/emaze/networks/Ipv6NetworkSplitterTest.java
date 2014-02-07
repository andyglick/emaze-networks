package net.emaze.networks;

import java.util.List;
import org.junit.Test;

public class Ipv6NetworkSplitterTest {
    
    @Test
    public void shotgun() {
        final Ipv6Network net = Ipv6Network.fromCidrNotation("FFFF::/16");
        final List<Ipv6Network> result = new Ipv6NetworkSplitter().perform(net, 20);
        for(Ipv6Network n: result) {
            System.out.println(n);
        }
    }
    
}
