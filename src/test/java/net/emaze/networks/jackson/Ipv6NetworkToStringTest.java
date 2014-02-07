package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import net.emaze.networks.Ipv6Network;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6NetworkToStringTest {

    @Test
    public void testIPv6Serialization() throws IOException {
        final String expected = "{'cidr':'1234:0000:0000:0000:0000:0000:0000:0000/16'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6Cidr bwc = new BeanWithIpv6Cidr();
        bwc.setCidr(Ipv6Network.fromCidrNotation("1234::/16"));
        final String got = mapper.writeValueAsString(bwc);
        Assert.assertEquals(expected, got);
    }
}
