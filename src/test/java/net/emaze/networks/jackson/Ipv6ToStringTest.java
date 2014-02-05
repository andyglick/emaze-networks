package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6;
import org.junit.Test;

public class Ipv6ToStringTest {

    @Test
    public void testIPv6Serialization() throws IOException {
        final String expected = "{'ip':'1234:0000:0000:0000:0000:0000:0000:0001'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6 bwi = new BeanWithIpv6();
        bwi.setIp(Ipv6.parse("1234::1"));
        final String got = mapper.writeValueAsString(bwi);
        Assert.assertEquals(expected, got);
    }
}